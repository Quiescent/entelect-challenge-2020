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
         (my-abs-x      (my-abs-x state))
         (boosting      (im-boosting state))
         (boosts        (my-boosts state))
         (speed         (my-speed state)))
    (determine-move map my-pos boosting boosts speed my-abs-x)))

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
  (count-if (lambda (x) (string-equal x "BOOST"))
            (deep-accessor this 'player 'powerups)))

(defmethod my-speed ((this state))
  "Produce the which I'm going in THIS state."
  (deep-accessor this 'player 'player-speed))

(defun determine-move (game-map my-pos boosting boosts speed my-abs-x)
  "Produce the best move for GAME-MAP.

Given that I'm at MY-POS, whether I'm BOOSTING, how many BOOSTS I have
left, the SPEED at which I'm going and MY-ABS-X position on the
board."
  (declare (ignore my-abs-x boosting))
  (bind ((end-states         (states-from game-map my-pos speed boosts))
         (fewest-moves       (only-shortest-path-length end-states))
         (best-by-prediction (-> (copy-seq fewest-moves)
                               (sort #'> :key (lambda (state) (nth 3 state)))
                               (stable-sort #'<
                                            :key (lambda (state)
                                                   (variance-score
                                                    state
                                                    (array-dimension game-map 1))))
                               (stable-sort #'>
                                            :key (lambda (state)
                                                   (best-median-distance-score
                                                    state
                                                    (array-dimension game-map 1))))
                               caar
                               last
                               car)))
    best-by-prediction))

(defmacro distance-score ()
  "Produce an expression modelling the best distance into random maps."
  (with-open-file (file "model.csv")
    `(cond
       ,@(iter
           (with line)
           (for (speed x y boosts . scores) = (->> (read-line file nil)
                                                (ppcre:split ",")
                                                (mapcar #'read-from-string)
                                                (setq line)))
           (while line)
           (collecting `((and (= ,speed  speed)
                              (= ,x      x)
                              (= ,y      y)
                              (= ,boosts boosts))
                         ,(apply #'max scores)))))))

(defun best-median-distance-score (state map-length)
  "Produce the score of STATE according to the model in model.csv.

Use MAP-LENGTH to compute the actual X value."
  (bind (((_ (x-prelim . y) speed prelim-boosts) state)
         (x                                      (- x-prelim map-length))
         (boosts                                 (if (> prelim-boosts 0) 1 0)))
    (if (= 0 speed)
        -1
        (distance-score))))

(eval-when (:compile-toplevel
            :load-toplevel
            :execute)
  (defun standard-deviation-ignoring-minus-ones (xs)
    "Compute the standard deviation of XS ignoring -1 values."
    (bind ((without-minus-one (remove -1 xs))
           (average           (-<> without-minus-one
                                (apply #'+ <>)
                                (/ <> (length without-minus-one))
                                (float <>))))
      (-<> without-minus-one
        (mapcar (lambda (x) (bind ((diff (- x average))) (* diff diff))) <>)
        (apply #'+ <>)
        (/ <> (1- (length without-minus-one)))
        sqrt))))

(defmacro variance-model ()
  "Produce an expression modelling the best distance into random maps."
  (with-open-file (file "model.csv")
    `(cond
       ,@(iter
           (with line)
           (for (speed x y boosts . scores) = (->> (read-line file nil)
                                                (ppcre:split ",")
                                                (mapcar #'read-from-string)
                                                (setq line)))
           (while line)
           (collecting `((and (= ,speed  speed)
                              (= ,x      x)
                              (= ,y      y)
                              (= ,boosts boosts))
                         ,(standard-deviation-ignoring-minus-ones scores)))))))

(defun variance-score (state map-length)
  "Produce the score of STATE according to the model in model.csv.

Use MAP-LENGTH to compute the actual X value."
  (bind (((_ (x-prelim . y) speed prelim-boosts) state)
         (x                                      (- x-prelim map-length))
         (boosts                                 (if (> prelim-boosts 0) 1 0)))
    (if (= 0 speed)
        -1
        (variance-model))))

(defun only-shortest-path-length (end-states)
  "Produce only those END-STATES which took the shortest number of steps."
  (bind ((shortest-length (iter (for (path . _) in end-states) (minimize (length path)))))
    (remove-if (lambda (end-state) (> (length (car end-state)) shortest-length)) end-states)))

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
            (with possible-moves =
                  (remove-if (lambda (move) (or (not (move-can-be-made move
                                                                       current-boosts
                                                                       (cdr current-pos)))
                                                (and (= 0 current-speed)
                                                     (or (eq move 'turn_right)
                                                         (eq move 'turn_left)))))
                             all-moves))
            (for move in possible-moves)
            (bind (((:values new-pos new-speed new-boosts)
                    (make-move move game-map current-pos current-speed current-boosts)))
              (push (list (cons move path) new-pos new-speed new-boosts)
                    paths-to-explore)))))
    (incf counter)
    (finally (return found-paths))))

(defmacro ahead-of (type direction speed game-map pos)
  "Produce the appropriate `ahead-of' form.

Take into account the nuances of changing direction and not counting
the current square.  e.g. changing direction means that you travel
diagonally ignoring the squars on the rectalinear path.

If TYPE is 'SPEED then produce `speed-ahead-of' if it's 'MUD then
produce `mud-ahead-of'.

If DIRECTION is 'AHEAD then don't change direction.  A DIRECTION of
'UP corresponds to turning left and 'DOWN corresponds to turning
right.

SPEED, GAME-MAP, and POS should be un-adjusted values."
  (bind ((fun       (ecase type
                      (mud   'mud-ahead-of)
                      (speed 'speed-ahead-of)))
         (adj-speed (ecase direction
                      (up    `(- ,speed 2))
                      (down  `(- ,speed 2))
                      (ahead `(1- ,speed))))
         (adj-x     `(1+ (car ,pos)))
         (adj-y     (ecase direction
                      (up    `(1- (cdr ,pos)))
                      (down  `(1+ (cdr ,pos)))
                      (ahead `(cdr ,pos)))))
    `(,fun ,adj-speed ,game-map ,adj-x ,adj-y)))

(defmacro move-car (direction speed x)
  "Produce the new value of X when the car moves in DIRECTION at SPEED."
  `(+ ,x ,(ecase direction
            (up    `(1- ,speed))
            (down  `(1- ,speed))
            (ahead speed))))

(defmacro move-lat (direction y)
  "Produce the new value of Y when the car moves in DIRECTION."
  (ecase direction
    (up    `(1- ,y))
    (down  `(1+ ,y))
    (ahead y)))

;; Known short cuts:
;;  - I don't take collisions with the other player into account;
;;  - I don't take boost length into account;
;;
;; TODO: If the other players move is supplied then check for
;;       collisions.
(defun make-move (move game-map position speed boosts)
  "Make MOVE across GAME-MAP from POSITION at SPEED with BOOSTS.

Produce the new new position, etc. as values."
  (bind ((new-speed   (case move
                        (accelerate (increase-speed speed))
                        (use_boost  15)
                        (otherwise  speed)))
         ((x . y)     position)
         (new-x       (case move
                        (turn_left  (move-car up    new-speed x))
                        (turn_right (move-car down  new-speed x))
                        (otherwise  (move-car ahead new-speed x))))
         (new-y       (case move
                        (turn_left  (move-lat up    y))
                        (turn_right (move-lat down  y))
                        (otherwise  (move-lat ahead y))))
         (new-pos     (cons new-x new-y))
         (muds-hit    (case move
                        (turn_left  (ahead-of mud up    new-speed game-map position))
                        (turn_right (ahead-of mud down  new-speed game-map position))
                        (otherwise  (ahead-of mud ahead new-speed game-map position))))
         (new-boosts  (case move
                        (turn_left  (ahead-of speed up    new-speed game-map position))
                        (turn_right (ahead-of speed down  new-speed game-map position))
                        (use_boost  (1- (ahead-of speed ahead new-speed game-map position)))
                        (otherwise  (ahead-of speed ahead new-speed game-map position))))
         (final-speed (decrease-speed-by muds-hit new-speed)))
    (values new-pos final-speed (+ new-boosts boosts))))

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
    (3 6)
    (5 6)
    (6 8)
    (8 9)
    (t speed)))

(defun decrease-speed (speed)
  "Produce the speed which is one slower than SPEED."
  (case speed
    (3  3)
    (5  3)
    (6  3)
    (8  6)
    (9  8)
    (15 9)
    (t speed)))

(defun end-state (position game-map)
  "Produce T if POSITION, is considered an end state for the search in GAME-MAP."
  (>= (car position) (array-dimension game-map 1)))

(defun move-can-be-made (move boosts y)
  "Produce T if the MOVE can be made from Y coordinate."
  (case move
    (turn_right (< y 3))
    (turn_left  (> y 0))
    (use_boost  (> boosts 0))
    (t          t)))

(defun mud-ahead-of (speed game-map x y)
  "Produce the count of mud on SPEED blocks of GAME-MAP ahead of (X, Y)."
  (iter
    (for i from (max x 0) below (min (1+ (+ x speed)) (array-dimension game-map 1)))
    (counting (member (aref game-map y i) '(mud oil-spill)))))

(defun speed-ahead-of (speed game-map x y)
  "Produce the count of boosts on SPEED blocks of GAME-MAP ahead of (X, Y)."
  (iter
    (for i from (max x 0) below (min (1+ (+ x speed)) (array-dimension game-map 1)))
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

(defmethod my-abs-x ((this state))
  "Produce my absolute x position in THIS state."
  (car (position-to-cons (deep-accessor this 'player 'map-position))))

(defmethod my-abs-pos ((this state))
  "Produce my absolute position in THIS state."
  (to-zero-indexed (position-to-cons (deep-accessor this 'player 'map-position))))

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

(defmacro with-consecutive-states (path player which-player &rest body)
  "Load states from PATH for PLAYER and bind the current and next over BODY.

Prepend 'WHICH-PLAYER - ' to the path for the players move and state."
  `(iter
     (for round from 1)
     (when (or (not (directory (make-pathname :directory
                                              (list :relative
                                                    ,path
                                                    (format nil "Round ~3,'0d" round)
                                                    (format nil "~s - ~a" ,which-player ,player)))))
               (not (directory (make-pathname :directory
                                              (list :relative
                                                    ,path
                                                    (format nil "Round ~3,'0d" (1+ round))
                                                    (format nil "~s - ~a" ,which-player ,player))))))
       (terminate))
     (bind ((current-path  (format nil
                                   "~a/Round ~3,'0d/~s - ~a/JsonMap.json"
                                   ,path
                                   round
                                   ,which-player
                                   ,player))
            (move-path     (format nil
                                   "~a/Round ~3,'0d/~s - ~a/PlayerCommand.txt"
                                   ,path
                                   round
                                   ,which-player
                                   ,player))
            (current-state (load-state-from-file current-path))
            (current-move  (load-command-from-file move-path))
            (next-path     (format nil
                                   "~a/Round ~3,'0d/~s - ~a/JsonMap.json"
                                   ,path
                                   (1+ round)
                                   ,which-player
                                   ,player))
            (next-state    (load-state-from-file next-path)))
       ,@body)))

(defun load-command-from-file (file-path)
  "Load the players last command as a symbol from FILE-PATH."
  (with-open-file (f file-path)
    (read-from-string (cadr (cl-ppcre:split "Command: " (read-line f))))))

(defmacro should-equal (form-1 form-2)
  "Procude T if FORM-1 `equal's FORM-2.

If they're not equal then pretty print both forms."
  `(or (equal ,form-1 ,form-2)
       ,(print-side-by-side form-1 form-2)))

(defun print-side-by-side (form-1 form-2)
  "Pretty print FORM-1 and FORM-2 next to each other."
  (bind ((pp-form-1    (bind ((filler-1 (make-array '(0) :element-type 'base-char
                                                         :fill-pointer 0 :adjustable t)))
                         (with-output-to-string (stream filler-1)
                           (pprint form-1 stream))
                         filler-1))
         (form-1-lines (cl-ppcre:split "\\n" pp-form-1))
         (max-length-1 (apply #'max (mapcar #'length form-1-lines)))
         (pp-form-2    (bind ((filler-2 (make-array '(0) :element-type 'base-char
                                                         :fill-pointer 0 :adjustable t)))
                         (with-output-to-string (stream filler-2)
                           (pprint form-2 stream))
                         filler-2))
         (form-2-lines (cl-ppcre:split "\\n" pp-form-2)))
    (iter
      (for i from 0 below (max (length form-1-lines) (length form-2-lines)))
      (format t "~a" (or (nth i form-1-lines) ""))
      (format t (concatenate 'string "~" (format nil "~a" (+ 1 max-length-1)) "T| "))
      (format t "~a~%" (or (nth i form-2-lines) "")))))

#+nil
(defvar empty-game-map
  (iter
    (with game-map = (rows (load-state-from-file "state.json")))
    (for y from 0 below (array-dimension game-map 0))
    (iter
      (for x from 0 below (array-dimension game-map 1))
      (setf (aref game-map y x) nil))
    (finally (return game-map)))
  "An empty game map for testing purposes.")

(defun replay-from-folder (folder-path)
  "Check that `make-move' produces the same result as the target engine."
  (with-consecutive-states folder-path "Quantum" 'A
    (bind (((:values new-relative-pos new-speed new-boosts)
            (make-move current-move
                       (rows current-state)
                       (car (positions current-state))
                       (my-speed current-state)
                       (my-boosts current-state)))
           (my-abs-pos (my-abs-pos current-state))
           (new-pos    (cons (- (+ (car my-abs-pos) (car new-relative-pos))
                                (if (eq round 1) 0 5))
                             (cdr new-relative-pos)))
           (initial    (list (my-abs-pos current-state)
                             (my-speed current-state)
                             (my-boosts current-state)))
           (computed   (list new-pos new-speed new-boosts))
           (actual     (list (my-abs-pos next-state)
                             (my-speed next-state)
                             (my-boosts next-state))))
      (when (not (equal computed actual))
        (format t "~s:~6T~a ~25T ~a ~40T~a /~65T~a~%"
                round
                initial
                current-move
                computed
                actual)))))
