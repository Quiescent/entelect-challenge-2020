(in-package :bot)

(defun main ()
  (iter
    (while t)
    (for round-number = (read-line))
    (for move = (move-for-round round-number))
    (format t "C;~a;~a~%" round-number (move-to-string move))))

(defun move-to-string (move)
  "Produce a string representation of MOVE."
  (if (consp move)
      (format nil "~a ~a ~a" (car move) (cddr move) (cadr move))
      (format nil "~a" move)))

(defun move-for-round (round-number)
  "Produce a move which is appropriate for ROUND-NUMBER."
  (bind ((state             (load-state-file round-number))
         (map               (rows state))
         ((my-pos . op-pos) (positions state))
         (my-abs-x          (my-abs-x state))
         (opponent-abs-x    (opponent-abs-x state))
         (boosting          (im-boosting state))
         (boosts            (my-boosts state))
         (lizards           (my-lizards state))
         (trucks            (my-trucks state))
         (speed             (my-speed state))
         (op-boosts         1)
         (op-speed          (opponent-speed state)))
    (determine-move map
                    my-pos
                    boosting
                    boosts
                    lizards
                    trucks
                    speed
                    my-abs-x
                    opponent-abs-x
                    op-pos
                    op-boosts
                    op-speed)))

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

(defmethod my-lizards ((this state))
  "Produce the number of boosts which I have in THIS state."
  (count-if (lambda (x) (string-equal x "LIZARD"))
            (deep-accessor this 'player 'powerups)))

(defmethod my-trucks ((this state))
  "Produce the number of boosts which I have in THIS state."
  (count-if (lambda (x) (string-equal x "TWEET"))
            (deep-accessor this 'player 'powerups)))

(defmethod my-speed ((this state))
  "Produce the speed which I'm going in THIS state."
  (deep-accessor this 'player 'player-speed))

(defmethod opponent-speed ((this state))
  "Produce the speed which the opponent is going in THIS state."
  (deep-accessor this 'opponent 'player-speed))

(defun determine-move (game-map my-pos boosting boosts
                       lizards trucks speed
                       my-abs-x opponent-abs-x
                       opponent-pos opponent-boosts
                       opponent-speed)
  "Produce the best move for GAME-MAP.

Given that I'm at MY-POS, whether I'm BOOSTING, how many BOOSTS,
LIZARDS and TRUCKS I have left, the SPEED at which I'm going and
MY-ABS-X position on the board."
  (declare (ignore boosting))
  (cond
    ((opponent-is-close-by my-abs-x (cdr my-pos) opponent-abs-x (cdr opponent-pos))
     (make-opposed-move game-map
                        my-pos
                        boosts
                        lizards
                        trucks
                        speed
                        opponent-pos
                        opponent-boosts
                        opponent-speed))
    (t
     (make-speed-move game-map my-pos boosts lizards trucks speed))))

(defun place-cyber-truck (opponent-abs-x opponent-abs-y)
  "Produce a move which place the Cyber Truck in front of the opponent.

The opponent is at the _absolute_ coordinate:
(opponent-abs-x, opponent-abs-y)."
  ;; Add one to y because their coordinates are 1-based
  (cons 'use_tweet (cons (1+ opponent-abs-x) (1+ opponent-abs-y))))

(defmacro cannot-make-move (boosts lizards trucks pos)
  "Produce a function which produces T if MOVE can't be made with BOOSTS from POS."
  `(lambda (move) (not (move-can-be-made move ,boosts ,lizards ,trucks (cdr ,pos)))))

(defun remove-impossible-moves (boosts lizards trucks pos all-moves)
  "Remove impossible moves from ALL-MOVES.

Given that the player has BOOSTS, LIZARDS and TRUCKS left and is at
POS."
  (remove-if (cannot-make-move boosts lizards trucks pos) all-moves))

(defconstant maximax-depth 3
  "The depth that we should search the game tree.")

(defun make-opposed-move (game-map my-pos boosts
                          lizards trucks speed
                          op-pos op-boosts op-speed)
  "Produce the best move on GAME-MAP as determined by a few rounds of maximax.

The optimiser is run with my bot at MY-POS, with BOOSTS, LIZARDS and
TRUCKS remaining running at SPEED and the opponent running from OP-POS
with OP-BOOSTS at OP-SPEED."
  (caddr (make-opposed-move-iter game-map
                                 my-pos
                                 boosts
                                 lizards
                                 trucks
                                 speed
                                 op-pos
                                 op-boosts
                                 op-speed
                                 maximax-depth)))

(defun maximax-score (turns-to-end x-pos speed)
  "Produce a score for a state in maximax.

Score weights the TURNS-TO-END of the current map most highly and then
breaks ties on the X-POS and then finally on the SPEED."
  (+ (if (/= turns-to-end -1) (* 10000 turns-to-end) 0)
     (* 100 x-pos)
     speed))

(defvar all-moves '(accelerate use_boost turn_right turn_left nothing decelerate use_tweet use_lizard)
  "All the moves which I can make.")

(defun make-opposed-move-iter (game-map my-pos boosts lizards trucks speed
                               op-pos op-boosts op-speed count)
  "Find a good move against the opponent which gets me out ahead of him."
  (iter
    (for cell
         in (iter
              (for my-move in (remove-impossible-moves boosts lizards trucks my-pos all-moves))
              (collecting
               (bind (((:values my-pos-2 my-speed-2 my-boosts-2 my-lizards-2 my-trucks-2)
                       (make-move my-move game-map my-pos speed boosts trucks speed)))
                 (iter inner
                   ;; Assume that the opponent always has powerups
                   (for op-move in (remove-impossible-moves 1 1 1 op-pos all-moves))
                   (bind (((:values op-pos-2 op-speed-2 op-boosts-2)
                           (make-move op-move
                                      game-map
                                      op-pos
                                      op-speed
                                      op-boosts
                                      1
                                      1))
                          (my-resolved-pos-2   (resolve-collisions my-pos
                                                                   op-pos
                                                                   my-pos-2
                                                                   op-pos-2))
                          (op-resolved-pos-2   (resolve-collisions op-pos
                                                                   my-pos
                                                                   op-pos-2
                                                                   my-pos-2))
                          (turns-to-end        (if (end-state my-resolved-pos-2 game-map)
                                                   count
                                                   -1))
                          (op-turns-to-end     (if (end-state my-resolved-pos-2 game-map)
                                                   count
                                                   -1))
                          ((my-score
                            op-score
                            _)                 (if (or (/= turns-to-end -1)
                                                       (= count 1))
                            (list (maximax-score turns-to-end
                                                 (car my-resolved-pos-2)
                                                 my-speed-2)
                                  (maximax-score op-turns-to-end
                                                 (car op-resolved-pos-2)
                                                 op-speed-2)
                                  nil)
                            (make-opposed-move-iter game-map
                                                    my-resolved-pos-2
                                                    my-boosts-2
                                                    my-lizards-2
                                                    my-trucks-2
                                                    my-speed-2
                                                    op-resolved-pos-2
                                                    op-boosts-2
                                                    op-speed-2
                                                    (1- count)))))
                     (finding (list my-score op-score my-move)
                              maximizing op-score)))))))
    (finding cell maximizing (car cell))))

(defun game-map-y-dim (game-map)
  "Produce the number of squares in the y dimension of GAME-MAP."
  (array-dimension (car game-map) 0))

(defun game-map-x-dim (game-map)
  "Produce the number of squares in the x dimension of GAME-MAP."
  (array-dimension (car game-map) 1))

(defun make-speed-move (game-map my-pos boosts lizards trucks speed)
  "Produce the best SPEED map for GAME-MAP.

Given that I'm at MY-POS, ow many BOOSTS, LIZARDS and TRUCKS I have
left, the SPEED at which I'm going and MY-ABS-X position on the
board."
  (-> (states-with-fewest-moves game-map my-pos boosts lizards trucks speed)
    only-those-which-dont-slow
    copy-seq
    (sort #'> :key (lambda (state) (nth 3 state)))
    (stable-sort #'<
                 :key (lambda (state)
                        (variance-score
                         state
                         (game-map-x-dim game-map))))
    (stable-sort #'>
                 :key (lambda (state)
                        (best-median-distance-score
                         state
                         (game-map-x-dim game-map))))
    caar
    last
    car))

(defun only-those-which-dont-slow (end-states)
  "Filter END-STATES to those which don't lose speed or lose least.

Given that the car was going at INITIAL-SPEED originally."
  (bind ((fastest
          (iter
            (for state in end-states)
            (maximizing (nth 2 state)))))
    (iter
      (for state in end-states)
      (when (>= (nth 2 state) fastest)
        (collecting state)))))

(defun states-with-fewest-moves (game-map my-pos boosts lizards trucks speed)
  "Produce the states with the shortest paths to the end of the GAME-MAP."
  (bind ((end-states   (states-from game-map my-pos speed boosts lizards trucks))
         (fewest-moves (only-shortest-path-length end-states)))
    fewest-moves))

(defconstant window-to-consider-maximax 3
  "The window around me that I should use to consider using maximax.")

(defun opponent-is-close-by (my-abs-x my-y opponent-abs-x opponent-y)
  "Produce t if MY-ABS-X is at a position where I can see OPPONENT-ABS-X."
  (and (>= opponent-abs-x (- my-abs-x window-to-consider-maximax))
       (<= opponent-abs-x (+ my-abs-x window-to-consider-maximax))
       (>= opponent-y     (- my-y     1))
       (<= opponent-y     (+ my-y     1))))

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

;; TODO: Check.  There's something wrong here...
(defun states-from (game-map my-pos speed boosts lizards trucks)
  "Produce all possible states using GAME-MAP.

Where my car is at MY-POS and is going at SPEED and I have BOOSTS,
LIZARDS and TRUCKS left.

First element of a path is the path taken.
Second is my current position.
Third is my speed.
Fourth is my boosts left."
  (iter
    (with shortest-path = most-positive-fixnum)
    (with counter = 0)
    (with paths-to-explore = (list (list nil my-pos speed boosts lizards trucks)))
    (with explored = (make-hash-table :test #'equal))
    (with found-paths)
    (while (and (not (null paths-to-explore))
                (< counter 100000)))
    (bind (((path current-pos current-speed current-boosts current-lizards current-trucks)
            (pop paths-to-explore)))
      (when (or (gethash path explored)
                (> (length path) shortest-path))
        (next-iteration))
      (if (end-state current-pos game-map)
          (progn
            (setf shortest-path (min shortest-path (length path)))
            (push (list path current-pos current-speed current-boosts)
                 found-paths))
          (iter
            (for move in (remove-impossible-moves current-boosts
                                                  current-lizards
                                                  current-trucks
                                                  current-pos
                                                  all-moves))
            (bind (((:values new-pos new-speed new-boosts new-lizards new-trucks)
                    (make-move move game-map current-pos current-speed current-boosts current-lizards current-trucks)))
              (push (list (cons move path) new-pos new-speed new-boosts new-lizards new-trucks)
                    paths-to-explore)))))
    (incf counter)
    (finally (return found-paths))))

(defmacro ahead-of (move type speed game-map pos)
  "Produce the appropriate `ahead-of' form.

Take into account the nuances of changing direction and not counting
the current square.  e.g. changing direction means that you travel
diagonally ignoring the squars on the rectalinear path.

MOVE can modify where we look.  For instance a MOVE of LIZARD means
that we need to only look at the square which we land on.

If TYPE is 'SPEED then produce `boost-ahead-of' if it's 'MUD then
produce `mud-ahead-of' etc.

SPEED, GAME-MAP, and POS should be un-adjusted values."
  (bind ((fun       (ecase type
                      (mud    'mud-ahead-of)
                      (boost  'boost-ahead-of)
                      (wall   'wall-ahead-of)
                      (tweet  'tweet-ahead-of)
                      (lizard 'lizard-ahead-of)))
         (adj-speed `(if (eq ,move 'use_lizard)
                         0
                         (1- ,speed)))
         (direction `(case ,move
                       (turn_left  'up)
                       (turn_right 'down)
                       (otherwise  'ahead)))
         (adj-x     `(if (eq ,move 'use_lizard)
                         (+ ,speed (car ,pos))
                         (ecase ,direction
                           (up    (car ,pos))
                           (down  (car ,pos))
                           (ahead (1+ (car ,pos))))))
         (adj-y     `(ecase ,direction
                       (up    (1- (cdr ,pos)))
                       (down  (1+ (cdr ,pos)))
                       (ahead (cdr ,pos)))))
    `(,fun ,adj-speed ,game-map ,adj-x ,adj-y)))

;; Paul Graham: On Lisp
(eval-when (:compile-toplevel
            :load-toplevel
            :execute)
  (defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

  (defun symb (&rest args)
    (values (intern (apply #'mkstr args)))))

(defmacro defun-ahead-of (type)
  "Produce a function which counts the number of TYPE blocks.

On a given game-map when travelling at a given speed from a given
coordinate."
  `(defun ,(symb type '-ahead-of) (speed game-map x y)
     ,(concatenate 'string "Produce the count of " (symbol-name type) " blocks of GAME-MAP ahead of (X, Y).")
     (iter
       (for i from (max x 0) below (min (1+ (+ x speed)) (game-map-x-dim game-map)))
       (counting (eq (quote ,type) (aref-game-map game-map y i))))))

(defun-ahead-of boost)
(defun-ahead-of wall)
(defun-ahead-of lizard)
(defun-ahead-of tweet)

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

(eval-when (:compile-toplevel
            :load-toplevel
            :execute)
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
      (t speed))))

(defmacro new-speed (move speed)
  "Produce the speed which MOVE will change SPEED to."
  `(case ,move
     (accelerate (increase-speed ,speed))
     (decelerate (decrease-speed ,speed))
     (use_boost  15)
     (otherwise  ,speed)))

(defmacro new-x (x move speed)
  "Produce the new value of X when MOVE is made at SPEED."
  `(case ,move
     (turn_left  (move-car up    ,speed ,x))
     (turn_right (move-car down  ,speed ,x))
     (otherwise  (move-car ahead ,speed ,x))))

(defmacro new-y (y move)
  "Produce the new value of Y when MOVE is made."
  `(case ,move
     (turn_left  (move-lat up    ,y))
     (turn_right (move-lat down  ,y))
     (otherwise  (move-lat ahead ,y))))

(defun manual-decelerate (speed)
  "Decelerate from SPEED as an action."
  (if (= speed 3)
      0
      (decrease-speed speed)))

(defmacro accumulating-powerups (count-name move type speed game-map position)
  "Produce the value of COUNT-NAME with collected powerups.

Use the state transitions which occur when MOVE is made, finding
powerups of TYPE on the GAME-MAP starting from POSITION."
  `(+ ,count-name
      (if (eq ,move (quote ,(symb 'use_ type))) -1 0)
      (ahead-of ,move ,type ,speed ,game-map ,position)))

;; Known short cuts:
;;  - I don't take boost length into account;
(defun make-move (move game-map position speed boosts lizards trucks)
  "Make MOVE across GAME-MAP from POSITION at SPEED with BOOSTS.

Produce the new new position, etc. as values."
  (bind ((new-speed   (new-speed move speed))
         ((x . y)     position)
         (new-x       (new-x x move new-speed))
         (new-y       (new-y y move))
         (new-pos     (cons new-x new-y))
         (muds-hit    (ahead-of                      move mud    new-speed game-map position))
         (walls-hit   (ahead-of                      move wall   new-speed game-map position))
         (new-boosts  (accumulating-powerups boosts  move boost  new-speed game-map position))
         (new-lizards (accumulating-powerups lizards move lizard new-speed game-map position))
         (new-trucks  (accumulating-powerups trucks  move tweet  new-speed game-map position))
         (final-speed (if (> walls-hit 0) 3 (decrease-speed-by muds-hit new-speed))))
    (values new-pos final-speed new-boosts new-lizards new-trucks)))

(defun decrease-speed-by (times speed)
  "Reduce SPEED TIMES times."
  (iter
    (with final-speed = speed)
    (for i from 0 below times)
    (setf final-speed (decrease-speed final-speed))
    (finally (return final-speed))))

(defun end-state (position game-map)
  "Produce T if POSITION, is considered an end state for the search in GAME-MAP."
  (>= (car position) (game-map-x-dim game-map)))

(defun move-can-be-made (move boosts lizards trucks y)
  "Produce T if the MOVE can be made from Y coordinate."
  (case move
    (turn_right (< y 3))
    (turn_left  (> y 0))
    (use_boost  (> boosts 0))
    (use_tweet  (> trucks 0))
    (use_lizard (> lizards 0))
    (t          t)))

(defun aref-game-map (game-map y x)
  "Produce the value in GAME-MAP at coordinate Y, X."
  (aref (car game-map) y x))

(defun mud-ahead-of (speed game-map x y)
  "Produce the count of mud on SPEED blocks of GAME-MAP ahead of (X, Y)."
  (iter
    (for i from (max x 0) below (min (1+ (+ x speed)) (game-map-x-dim game-map)))
    (counting (member (aref-game-map game-map y i) '(mud oil-spill)))))

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
              (5 'boost)
              (6 'wall)
              (7 'lizard)
              (8 'tweet))))
    (finally (return (cons result nil))))) ;; TODO cdr should be trucks

(defmethod position-to-cons ((this map-position))
  "Produce a cons cell of postions from THIS position."
  (cons (slot-value this 'x)
        (slot-value this 'y)))

(defun to-relative-position (pos min-x)
  "Convert POS to a relative position, given that MIN-X is the smallest x on the map."
  (->> pos
    position-to-cons
    (subtract-x min-x)
    to-zero-indexed))

(defmethod positions ((this state))
  "Produce the player positions in THIS state."
  (bind ((min-x (minimum-x this)))
    (cons (to-relative-position (deep-accessor this 'player   'map-position) min-x)
          (to-relative-position (deep-accessor this 'opponent 'map-position) min-x))))

(defmethod opponent-abs-x ((this state))
  "Produce opponents absolute x position in THIS state."
  (car (position-to-cons (deep-accessor this 'opponent 'map-position))))

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
            (op-path       (->> (directory (format nil "~a/Round ~3,'0d/*" ,path round))
                             (mapcar #'namestring)
                             (remove-if (lambda (path) (ppcre:all-matches ,player path)))
                             car))
            (op-move-path  (format nil
                                   "~a/PlayerCommand.txt"
                                   op-path))
            (current-state (load-state-from-file current-path))
            (current-move  (load-command-from-file move-path))
            (opponent-move (load-command-from-file op-move-path))
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
    (for y from 0 below (game-map-y-dim game-map))
    (iter
      (for x from 0 below (game-map-x-dim game-map))
      (setf (aref (car game-map) y x) nil))
    (finally (return game-map)))
  "An empty game map for testing purposes.")

(defun resolve-collisions (my-orig-pos opponent-orig-pos my-pos opponent-pos)
  "Resolve collisions and produce the new position of my car.

Given MY-ORIG-POS and the OPPONENT-ORIG-POS which we were in, MY-POS
after my move and the OPPONENT-POS after his/her move."
  (bind (((my-orig-x . my-orig-y) my-orig-pos)
         ((op-orig-x . op-orig-y) opponent-orig-pos)
         (i-got-ahead     (and (>= (car my-pos) (car opponent-pos))
                               (< my-orig-x op-orig-x)))
         (start-lane-same (= my-orig-y op-orig-y))
         (end-lane-same   (= (cdr my-pos) (cdr opponent-pos)))
         (side-collision   (equal my-pos opponent-pos)))
    (cond
      ((and i-got-ahead start-lane-same end-lane-same)
       (cons (1- (car opponent-pos)) (cdr my-pos)))
      (side-collision (cons (1- (car my-pos)) my-orig-y))
      (t my-pos))))

(defun replay-from-folder (folder-path)
  "Check that `make-move' produces the same result as the target engine."
  (with-consecutive-states folder-path "Quantum" 'A
    (bind (((:values new-relative-pos new-speed new-boosts)
            (make-move current-move
                       (rows current-state)
                       (car (positions current-state))
                       (my-speed current-state)
                       (my-boosts current-state)
                       (my-lizards current-state)
                       (my-trucks current-state)))
           ((:values opponent-pos opponent-speed opponent-boosts)
            (make-move opponent-move
                       (rows current-state)
                       (cdr (positions current-state))
                       (opponent-speed current-state)
                       ;; I don't know how many boosts, lizards or
                       ;; trucks my opponent has(!?)
                       0
                       0
                       0))
           ((my-orig-pos . opponent-orig-pos) (positions current-state))
           (resolved-relative-pos
            (resolve-collisions my-orig-pos
                                opponent-orig-pos
                                new-relative-pos
                                opponent-pos))
           (my-abs-pos (my-abs-pos current-state))
           (resolved-pos   (cons (- (+ (car my-abs-pos) (car resolved-relative-pos))
                                    (if (eq round 1) 0 5))
                                 (cdr resolved-relative-pos)))
           (initial    (list (my-abs-pos current-state)
                             (my-speed current-state)
                             (my-boosts current-state)))
           (computed   (list resolved-pos new-speed new-boosts))
           (actual     (list (my-abs-pos next-state)
                             (my-speed next-state)
                             (my-boosts next-state))))
      (declare (ignore opponent-speed opponent-boosts))
      (when (not (equal computed actual))
        (format t "~s:~6T~a ~25T ~a ~40T~a /~65T~a~%"
                round
                initial
                current-move
                computed
                actual)))))
