(in-package :bot)

(defun main ()
  (iter
    (while t)
    (for round-number = (read-line))
    (for move = (move-for-round round-number))
    (format t "C;~a;~a~%" round-number move)))

(defun move-for-round (round-number)
  "Produce a move which is appropriate for ROUND-NUMBER."
  (bind ((state         (load-state-file round-number))
         (map           (rows state))
         ((my-pos . _)  (positions state))
         (boosts        (my-boosts state))
         (speed         (my-speed state)))
    (determine-move map my-pos boosts speed)))

(defmacro deep-accessor (object &rest nested-slots)
  "Produce the value of OBJECT at the path defined by NESTED-SLOTS."
  (reduce (lambda (result next-name) `(slot-value ,result ,next-name))
          (reverse (cdr nested-slots))
          :initial-value `(slot-value ,object ,(car nested-slots))))

(defmethod im-boosting ((this state))
  "Produce t if im currently boosting in THIS state."
  (deep-accessor this 'player 'boosting))

(defmethod my-boosts ((this state))
  "Produce the number of boosts which I have in THIS state."
  (- (count-if (lambda (x) (string-equal x "BOOST"))
               (deep-accessor this 'player 'powerups))
     (deep-accessor this 'player 'boost-counter)))

(defmethod my-speed ((this state))
  "Produce the which I'm going in THIS state."
  (deep-accessor this 'player 'player-speed))

(defmacro decision-tree (&rest forms)
  "Create a series of nested conds from FORMS.

To terminate the tree you must supply an unquoted symbol, otherwise
it's assumed that you're supplying the next condition."
  (labels ((decision-tree-iter (forms)
             (logging-cond-iter (mapcar (lambda (form)
                                          (progn
                                            (when (equal 'quote (car form))
                                              (error "Use unquoted symbol to terminate a tree!"))
                                            (if (symbolp (cadr form))
                                                (list (car form) (cadr form))
                                                `(,(car form) ,(decision-tree-iter (cdr form))))))
                                        forms))))
    (decision-tree-iter forms)))

#+nil
(decision-tree
 ((< 2 3) ((eq 2 2) blah)
          ((eq 3 4) haha)))

;; Errors out deliberately...
#+nil
(decision-tree
 ((< 2 3) ((eq 2 2) 'blah)))

(eval-when (:compile-toplevel
            :load-toplevel
            :execute)
  (defun logging-cond-iter (forms)
    "Wrap forms in a `cond' and print the condition which succeeded.

NOTE: Implementation detail of `logging-cond."
    `(,@(reduce #'first-success
                (mapcar (lambda (form) `(,(car form) (progn (print (quote ,(car form)) *error-output*)
                                                            ,(cadr form))))
                        forms)
                :initial-value nil)))

  (defun first-success (result form)
  "Produce either RESULT if it's non nill, or cadr of FORM if car of FORM."
  `(or ,result
       (if ,(car form)
           ,(cadr form))))

#+nil
(first-success 't '((eq 2 3) blah)))

(defmacro logging-cond (&rest forms)
  "Wrap forms in a `cond' and print the condition which succeeded."
  (logging-cond-iter forms))

#+nil
(logging-cond
 ((t 3))
 ((eq 3 5) 4))

(defun determine-move (game-map my-pos boosts speed)
  "Produce the best move for GAME-MAP.

Given that I'm at MY-POS, whether I'm BOOSTING, how many BOOSTS I have
left and the SPEED at which I'm going."
  (bind ((end-states                                (states-from game-map my-pos speed boosts))
         (fewest-moves                              (only-shortest-path-length end-states))
         ((fast-move fast-x fast-speed fast-boosts) (best-by-speed fewest-moves))
         ((far-move  far-x  far-speed  far-boosts)  (best-by-dist  fewest-moves)))
    (progn
      (format t "States: ~s~%" fewest-moves)
      (format t "far-move: ~s ~s ~s" far-x far-speed far-boosts)
      (decision-tree
       ((= fast-x far-x)
        fast-move)
       (t far-move)))))

(defun only-shortest-path-length (end-states)
  "Produce only those END-STATES which took the shortest number of steps."
  (bind ((shortest-length (iter (for (path . _) in end-states) (minimize (length path)))))
    (remove-if (lambda (end-state) (> (length (car end-state)) shortest-length)) end-states)))

(defun best-by-speed (end-states)
  "Produce the best of END-STATES by the final speed."
  (iter
    (for (path (x . y) speed boosts) in end-states)
    (finding (list (car (last path)) x speed boosts) maximizing speed)))

(defun best-by-dist (end-states)
  "Produce the best of END-STATES by the final distance."
  (iter
    (for (path (x . y) speed boosts) in end-states)
    (finding (list (car (last path)) x speed boosts) maximizing x)))

;; Speeds:
;; MINIMUM_SPEED = 0
;; SPEED_STATE_1 = 3
;; INITIAL_SPEED = 5
;; SPEED_STATE_2 = 6
;; SPEED_STATE_3 = 8
;; MAXIMUM_SPEED = 9
;; BOOST_SPEED = 15
;;
;; Boost:
;;  - boosting bosts for 5 turns;
;;  - hitting something reduces speed to MAXIMUM_SPEED;
;;
;; Hitting Mud:
;;  - decelerates the car;

(defvar all-moves '(accelerate use_boost turn_right turn_left)
  "All the moves which I can make.")

(defun states-from (game-map my-pos speed boosts)
  "Produce all possible states using GAME-MAP.

Where my car is at MY-POS and is going at SPEED and I have BOOSTS
boosts left.

First element of a path is the path taken.
Second is my current position.
Third is my speed.
Fourth is my boosts left."
  (iter
    (with counter = 0)
    (with paths-to-explore = (list (list nil my-pos speed boosts)))
    (with explored = (make-hash-table :test #'equal))
    (with found-paths)
    (while (and (not (null paths-to-explore))
                (< counter 100000)))
    (bind (((path current-pos current-speed current-boosts) (pop paths-to-explore)))
      (when (gethash path explored)
        (next-iteration))
      (if (end-state current-pos game-map)
          (push (list path current-pos current-speed current-boosts)
                found-paths)
          (iter
            (for move in all-moves)
            (when (or (not (move-can-be-made move current-boosts (cdr current-pos)))
                      (and (= 0 current-speed)
                           (or (eq move 'turn_right)
                               (eq move 'turn_left))))
              (next-iteration))
            (bind (((:values new-pos new-speed new-boosts)
                    (make-move move game-map current-pos current-speed current-boosts)))
              (push (list (cons move path) new-pos new-speed new-boosts)
                    paths-to-explore)))))
    (incf counter)
    (finally (return found-paths))))

(defun make-move (move game-map position speed boosts)
  "Make MOVE across GAME-MAP from POSITION at SPEED with BOOSTS.

Produce the new new position, etc. as values."
  (case move
    (accelerate (bind ((new-speed   (increase-speed speed))
                       ((x . y)     position)
                       (new-pos     (cons (+ x new-speed) y))
                       (muds-hit    (mud-ahead-of new-speed game-map x y))
                       (final-speed (decrease-speed-by muds-hit new-speed)))
                  (values new-pos final-speed boosts)))
    (turn_left (bind (((x . y)     position)
                      (new-pos     (cons (+ x (1- speed)) (1- y)))
                      (muds-hit    (mud-ahead-of (1- speed) game-map x (1- y)))
                      (final-speed (decrease-speed-by muds-hit speed)))
                 (values new-pos final-speed boosts)))
    (turn_right (bind (((x . y)     position)
                       (new-pos     (cons (+ x (1- speed)) (1+ y)))
                       (muds-hit    (mud-ahead-of (1- speed) game-map x (1+ y)))
                       (final-speed (decrease-speed-by muds-hit speed)))
                  (values new-pos final-speed boosts)))
    (use_boost  (bind ((new-speed   15)
                       ((x . y)     position)
                       (new-pos     (cons (+ x new-speed) y))
                       (muds-hit    (mud-ahead-of new-speed game-map x y))
                       (final-speed (decrease-speed-by muds-hit new-speed)))
                  (values new-pos final-speed boosts)))))

(defun decrease-speed-by (times speed)
  "Reduce SPEED TIMES times."
  (iter
    (with final-speed = speed)
    (for i from 0 below times)
    (setf final-speed (decrease-speed final-speed))
    (finally (return final-speed))))

(defun increase-speed (speed)
  "Produce the speed which is one faster than SPEED."
  (case speed
    (0 3)
    (5 6)
    (6 8)
    (8 9)
    (t speed)))

(defun decrease-speed (speed)
  "Produce the speed which is one slower than SPEED."
  (case speed
    (3  0)
    (6  5)
    (8  6)
    (9  8)
    (15 9)
    (t speed)))

(defun end-state (position game-map)
  "Produce T if POSITION, is considered an end state for the search in GAME-MAP."
  (> (car position) (array-dimension game-map 1)))

(defun move-can-be-made (move boosts y)
  "Produce T if the MOVE can be made from Y coordinate."
  (case move
    (turn_right (< y 3))
    (turn_left  (> y 0))
    (use_boost  (> boosts 0))
    (t          t)))

(defun gap-ahead-of (speed game-map x y)
  "Produce T if there are SPEED clear blocks in GAME-MAP ahead of (X, Y)."
  (iter
    (for i from (max x 0) below (min (+ x speed) (array-dimension game-map 1)))
    (counting (not (eq 'mud (aref game-map y i))))))

(defconstant row-length 26
  "The number of squares visible in a row.")

(defun mud-ahead-of (speed game-map x y)
  "Produce the count of mud on SPEED blocks of GAME-MAP ahead of (X, Y)."
  (iter
    (for i from (max x 0) below (min (+ x speed) (array-dimension game-map 1)))
    (counting (eq 'mud (aref game-map y i)))))

(defun speed-ahead-of (speed game-map x y)
  "Produce the count of boosts on SPEED blocks of GAME-MAP ahead of (X, Y)."
  (iter
    (for i from (max x 0) below (min (+ x speed) (array-dimension game-map 1)))
    (counting (eq 'boost (aref game-map y i)))))

(defun load-state-file (round)
  "Load the state file for ROUND."
  (load-state-from-file (format nil "rounds/~a/state.json" round)))

(defun load-state-from-file (file-path)
  "Load the state from file at FILE-PATH."
  (with-open-file (f file-path)
    (parse-state f)))

(defmethod rows ((this state))
  "Produce rows as a 2D array of cells from the map in THIS state."
  (iter
    (with world-map = (slot-value this 'world-map))
    (with result = (make-array (list (length world-map)
                                     (length (aref world-map 0)))
                               :initial-element nil))
    (for y from 0)
    (for row in-vector world-map)
    (iter
      (for cell in-vector row)
      (for x from 0)
      (setf (aref result y x)
            (case (slot-value cell 'surface-object)
              (0 nil)
              (1 'mud)
              (2 'oil-spill)
              (3 'oil-item)
              (4 'finish-line)
              (5 'boost))))
    (finally (return result))))

(defmethod position-to-cons ((this map-position))
  "Produce a cons cell of postions from THIS position."
  (cons (slot-value this 'x)
        (slot-value this 'y)))

(defmethod positions ((this state))
  "Produce the player positions in THIS state."
  (bind ((min-x (minimum-x this)))
    (cons (to-zero-indexed (subtract-x min-x (position-to-cons (deep-accessor this 'player   'map-position))))
          (to-zero-indexed (subtract-x min-x (position-to-cons (deep-accessor this 'opponent 'map-position)))))))

(defun to-zero-indexed (x-y)
  "Produce a zero-indexed version of X-Y."
  (bind (((x . y) x-y))
    (cons (1- x) (1- y))))

(defun subtract-x (x x-y)
  "Produce (X, Y) shifted left by X."
  (cons (- (car x-y) x) (cdr x-y)))

(defmethod minimum-x ((this state))
  "Produce the minimum value of x in the map in THIS state."
  (1- (deep-accessor (aref (aref (slot-value this 'world-map) 0) 0)
                     'map-position 'x)))
