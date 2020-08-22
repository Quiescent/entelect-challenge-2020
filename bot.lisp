(in-package :bot)

(defvar *heuristic-coeficients* '((1 1 1 1 1 1))
  "The coefficients to use when computing the score of a position.")

(defvar *current-turn* 1
  "The turn of the game that we're on.")

(defvar *previous-move* nil
  "The move which was made last turn.")

(defvar *full-game-map* (make-array '(4 1500) :initial-element nil)
  "The map as it's been discovered so far.")

(defvar *player-cyber-truck-position* nil
  "The position at which I last placed the cyber truck.")

(defun read-weights ()
  "Read all scores from the score config file."
  (with-open-file (f "./score-config")
    (iter
      (for next-member = (eval (ignore-errors (read f))))
      (while next-member)
      (collecting (mapcar (lambda (x) (coerce x 'float)) next-member)))))

(defun main ()
  (iter
    (while t)
    (initially
     (setf *heuristic-coeficients* (read-weights))
     (setf *full-game-map* (make-array '(4 1500) :initial-element nil))
     (setf *player-cyber-truck-position* nil))
    (for round-number = (read-line))
    (bind ((*current-turn* (read-from-string round-number)))
      (for move = (move-for-round round-number)))
    (format t "C;~a;~a~%" round-number (move-to-string move))))

(defun move-to-string (move)
  "Produce a string representation of MOVE."
  (if (consp move)
      (format nil "~a ~a ~a" (car move) (1+ (cddr move)) (cadr move))
      (format nil "~a" move)))

(defun move-for-round (round-number)
  "Produce a move which is appropriate for ROUND-NUMBER."
  (move-for-state (load-state-file round-number)))

(defvar *ahead-of-cache* nil
  "A cache of obstacles ahead of certain points.

Key is (speed x y).

Value is [muds boosts walls tweets lizards].")

;; Paul Graham: On Lisp
(eval-when (:compile-toplevel
            :load-toplevel
            :execute)

  (defun mkstr (&rest args)
    (with-output-to-string (s)
      (dolist (a args) (princ a s))))

  (defun symb (&rest args)
    (values (intern (apply #'mkstr args)))))

(defvar all-makeable-moves '(accelerate use_boost turn_right turn_left nothing decelerate use_lizard fix use_emp use_oil)
  "All the moves which I can make.")

(defvar all-straight-moves '(accelerate use_boost nothing decelerate)
  "All moves which will result in going straight without jumping.")

;; Might want to try turning on optimsations for this to get to depth
;; three!
(defconstant maximax-depth 2
  "The depth that we should search the game tree.")

#+nil
(sb-sprof:with-profiling (:max-samples 1000
                          :report :flat
                          :loop t)
  (format t "Tick~%")
  (move-for-state (load-state-from-file "./scratch/JsonMap.json")))

(defmacro placing-oil (move position game-map &rest body)
  "If MOVE is 'USE_OIL then put oil on GAME-MAP at POSITION.

Runs BODY and then restores the map."
  `(if (and (eq ,move 'use_oil)
            (< (car ,position) (game-map-x-dim ,game-map)))
       (progn
         (bind (((x . y)       ,position)
                (original-tile (aref-game-map ,game-map y x)))
           (setf-game-map ,game-map y x 'mud)
           ,@body
           (setf-game-map ,game-map y x original-tile)))
       ,@body))

;; The moves being made here don't make sense!
(defmacro make-opposed-move (game-state)
  "Produce the best move in GAME-STATE as determined by a few rounds of maximax."
  `(bind ((*ahead-of-cache*    (make-hash-table :test #'equal))
          (my-cyber-truck-move (most-used-square-on-shortest-paths ,game-state)))
     (declare (optimize (speed 3) (safety 0) (debug 0)))
     (with-initial-state ,(cons `(iteration (count ,maximax-depth)) game-state)
       (iter
         (for current-turn = (+ *current-turn* (- maximax-depth (iteration count))))
         (with cells =
               (iter
                 (for player-move in (if (= (iteration count) ,maximax-depth)
                                         (cons my-cyber-truck-move (player moves))
                                         (player moves)))
                 (collecting
                  (iter
                    (for opponent-move in (opponent moves))
                    (make-moves
                     player-move
                     opponent-move
                     (bind (((player-score opponent-score _ _ _)
                             (if (or (end-state (player position)   (game map))
                                     (end-state (opponent position) (game map))
                                     (< (iteration count) 1))
                                 (list (player score) (opponent score) nil nil nil)
                                 (recur (1- (iteration count))))))
                       (finding (list player-score
                                      opponent-score
                                      player-move
                                      opponent-move
                                      (- maximax-depth (iteration count)))
                                minimizing player-score)))))))
         (for cell in cells)
         (finding cell maximizing (car cell))))))

(defmacro with-initial-state (initial-state &rest body)
  "Conveniently advance INITIAL-STATE via BODY.

Use `with' to bind values.

Use `making-moves' to make a move and an opponent move.

Unused values will be ignored."
  (progn
    (when (not (assoc 'game-map initial-state))
      (error "Game map not specified"))
    (when (not (assoc 'player initial-state))
      (error "Player initial state not specified"))
    (when (not (assoc 'opponent initial-state))
      (error "Opponent initial state not specified"))
    (when (not (assoc 'game initial-state))
      (error "Game initial state not specified"))
    (bind ((player-state     (cdr (assoc 'player    initial-state)))
           (opponent-state   (cdr (assoc 'opponent  initial-state)))
           (game-state       (cdr (assoc 'game  initial-state)))
           (iteration-state  (when (assoc 'iteration initial-state)
                               (cdr (assoc 'iteration initial-state)))))
      (labels ((player-not-defined (symbol)
                 (when (not (assoc symbol player-state))
                   (error (concatenate 'string "Player " (symbol-name symbol) " not defined"))))
               (opponent-not-defined (symbol)
                 (when (not (assoc symbol opponent-state))
                   (error (concatenate 'string "Opponent " (symbol-name symbol) " not defined"))))
               (game-not-defined (symbol)
                 (when (not (assoc symbol game-state))
                   (error (concatenate 'string "Game " (symbol-name symbol) " not defined"))))
               (iteration-is-not-defined (symbol)
                 (when (not (assoc symbol iteration-state))
                   (error (concatenate 'string "Iteration " (symbol-name symbol) " not defined")))))
        (player-not-defined 'position)
        (player-not-defined 'boosts)
        (player-not-defined 'oils)
        (player-not-defined 'lizards)
        (player-not-defined 'trucks)
        (player-not-defined 'emps)
        (player-not-defined 'speed)
        (player-not-defined 'damage)
        (player-not-defined 'boost-counter)

        (opponent-not-defined 'position)
        (opponent-not-defined 'boosts)
        (opponent-not-defined 'oils)
        (opponent-not-defined 'lizards)
        (opponent-not-defined 'trucks)
        (opponent-not-defined 'emps)
        (opponent-not-defined 'speed)
        (opponent-not-defined 'damage)
        (opponent-not-defined 'boost-counter)

        (when iteration-state
          (iteration-is-not-defined 'count))

        (game-not-defined 'turn)

        `(bind ((current-game-map ,@(cdr (assoc 'game-map initial-state)))

                (player-position      ,@(cdr (assoc 'position      player-state)))
                (player-boosts        ,@(cdr (assoc 'boosts        player-state)))
                (player-oils          ,@(cdr (assoc 'oils          player-state)))
                (player-lizards       ,@(cdr (assoc 'lizards       player-state)))
                (player-trucks        ,@(cdr (assoc 'trucks        player-state)))
                (player-emps          ,@(cdr (assoc 'emps          player-state)))
                (player-speed         ,@(cdr (assoc 'speed         player-state)))
                (player-damage        ,@(cdr (assoc 'damage        player-state)))
                (player-boost-counter ,@(cdr (assoc 'boost-counter player-state)))

                (opponent-position      ,@(cdr (assoc 'position      opponent-state)))
                (opponent-boosts        ,@(cdr (assoc 'boosts        opponent-state)))
                (opponent-oils          ,@(cdr (assoc 'oils          opponent-state)))
                (opponent-lizards       ,@(cdr (assoc 'lizards       opponent-state)))
                (opponent-trucks        ,@(cdr (assoc 'trucks        opponent-state)))
                (opponent-emps          ,@(cdr (assoc 'emps          opponent-state)))
                (opponent-speed         ,@(cdr (assoc 'speed         opponent-state)))
                (opponent-damage        ,@(cdr (assoc 'damage        opponent-state)))
                (opponent-boost-counter ,@(cdr (assoc 'boost-counter opponent-state)))

                (game-turn              ,@(cdr (assoc 'turn game-state)))

                (iteration-count        ,@(and iteration-state (cdr (assoc 'count iteration-state)))))
           (macrolet ((make-moves (player-move opponent-move &rest subsequent)
                        `(bind (((:values player-position-2
                                          player-boosts-2
                                          player-oils-2
                                          player-lizards-2
                                          player-trucks-2
                                          player-emps-2
                                          player-speed-2
                                          player-damage-2
                                          player-boost-counter-2

                                          opponent-position-2
                                          opponent-boosts-2
                                          opponent-oils-2
                                          opponent-lizards-2
                                          opponent-trucks-2
                                          opponent-emps-2
                                          opponent-speed-2
                                          opponent-damage-2
                                          opponent-boost-counter-2)
                                 (process-moves player-position
                                                player-boosts
                                                player-oils
                                                player-lizards
                                                player-trucks
                                                player-emps
                                                player-speed
                                                player-damage
                                                player-boost-counter

                                                opponent-position
                                                opponent-boosts
                                                opponent-oils
                                                opponent-lizards
                                                opponent-trucks
                                                opponent-emps
                                                opponent-speed
                                                opponent-damage
                                                opponent-boost-counter

                                                current-game-map

                                                ,player-move
                                                ,opponent-move)))
                           (bind ((current-game-map current-game-map)

                                  (player-position      player-position-2)
                                  (player-boosts        player-boosts-2)
                                  (player-oils          player-oils-2)
                                  (player-lizards       player-lizards-2)
                                  (player-trucks        player-trucks-2)
                                  (player-emps          player-emps-2)
                                  (player-speed         player-speed-2)
                                  (player-damage        player-damage-2)
                                  (player-boost-counter player-boost-counter-2)

                                  (opponent-position      opponent-position-2)
                                  (opponent-boosts        opponent-boosts-2)
                                  (opponent-oils          opponent-oils-2)
                                  (opponent-lizards       opponent-lizards-2)
                                  (opponent-trucks        opponent-trucks-2)
                                  (opponent-emps          opponent-emps-2)
                                  (opponent-speed         opponent-speed-2)
                                  (opponent-damage        opponent-damage-2)
                                  (opponent-boost-counter opponent-boost-counter-2)

                                  (game-turn (1+ game-turn)))
                             (progn ,@subsequent))))
                      (opponent   (symbol) (values   (case symbol
                                                       (x '(car opponent-position))
                                                       (y '(cdr opponent-position))
                                                       (score '(global-score
                                                                (car opponent-position)
                                                                game-turn
                                                                opponent-boosts
                                                                opponent-lizards
                                                                opponent-trucks
                                                                opponent-emps
                                                                (cdr opponent-position)
                                                                opponent-damage))
                                                       (moves '(remove-impossible-moves
                                                                opponent-boosts
                                                                opponent-oils
                                                                opponent-lizards
                                                                opponent-trucks
                                                                opponent-position
                                                                opponent-emps
                                                                all-makeable-moves))
                                                       (t (intern (mkstr 'opponent  '- symbol))))))
                      (game       (symbol) (case symbol
                                             (map 'current-game-map)))
                      (player     (symbol) (values   (case symbol
                                                       (x '(car player-position))
                                                       (y '(cdr player-position))
                                                       (score '(global-score
                                                                (car player-position)
                                                                game-turn
                                                                player-boosts
                                                                player-lizards
                                                                player-trucks
                                                                player-emps
                                                                (cdr player-position)
                                                                player-damage))
                                                       (moves '(remove-impossible-moves
                                                                player-boosts
                                                                player-oils
                                                                player-lizards
                                                                player-trucks
                                                                player-position
                                                                player-emps
                                                                all-makeable-moves))
                                                       (t (intern (mkstr 'player    '- symbol))))))
                      (setting    (name value)
                        `(setf ,name ,value))
                      (iteration  (symbol) (values   (intern (mkstr 'iteration '- symbol))))
                      (recur      (iteration-count) `(recur-inner current-game-map
                                                                  player-position
                                                                  player-boosts
                                                                  player-oils
                                                                  player-lizards
                                                                  player-trucks
                                                                  player-emps
                                                                  player-speed
                                                                  player-damage
                                                                  player-boost-counter
                                                                  opponent-position
                                                                  opponent-boosts
                                                                  opponent-oils
                                                                  opponent-lizards
                                                                  opponent-trucks
                                                                  opponent-emps
                                                                  opponent-speed
                                                                  opponent-damage
                                                                  opponent-boost-counter
                                                                  game-turn
                                                                  ,iteration-count)))
             (labels ((recur-inner (current-game-map
                                    player-position
                                    player-boosts
                                    player-oils
                                    player-lizards
                                    player-trucks
                                    player-emps
                                    player-speed
                                    player-damage
                                    player-boost-counter
                                    opponent-position
                                    opponent-boosts
                                    opponent-oils
                                    opponent-lizards
                                    opponent-trucks
                                    opponent-emps
                                    opponent-speed
                                    opponent-damage
                                    opponent-boost-counter
                                    game-turn
                                    iteration-count)
                        (progn ,@body)))
               (recur iteration-count))))))))

#+nil
(bind ((*ahead-of-cache* (make-hash-table :test #'equal)))
  (with-initial-state ((game (turn 10))
                       (iteration (count 3))
                       (game-map empty-game-map)
                       (player (position (cons 2 1))
                               (boosts 3)
                               (oils 1)
                               (lizards 4)
                               (trucks 5)
                               (speed 6)
                               (damage 0)
                               (boost-counter 8))
                       (opponent (position (cons 10 3))
                                 (boosts 11)
                                 (oils 20)
                                 (lizards 12)
                                 (trucks 13)
                                 (speed 14)
                                 (damage 0)
                                 (boost-counter 16)))
    (format t "~a~%" (player speed))
    (if (> (iteration count) 0)
        (progn
          (format t "Count: ~a~%" (iteration count))
          (make-moves 'accelerate 'accelerate
                      (format t "Speed: ~a~%" (player speed))
                      (recur (1- (iteration count)))))
        (progn (format t "Speed: ~a~%" (player speed))
               (format t "done!~%")))))

(defmacro make-speed-move (game-state)
  "Produce the best speed move to make on GAME-STATE."
  `(bind ((*ahead-of-cache* (make-hash-table :test #'equal)))
    (-> (rank-order-all-moves ,game-state)
      caar
      last
      car)))

(defmacro no-net-change (move game-state)
  "Produce t if MOVE didn't actively change GAME-STATE.

i.e. if we may as well not have mode MOVE."
  `(bind ((*ahead-of-cache* (make-hash-table :test #'equal)))
     (and (not (member ,move '(use_emp use_lizard use_boost use_tweet use_oil)))
          (with-initial-state ,game-state
            (bind (nothing-player-position
                   nothing-player-speed
                   nothing-player-damage)
              (make-moves
               'nothing
               'nothing
               (setf nothing-player-position      (player position)
                     nothing-player-speed         (player speed)
                     nothing-player-damage        (player damage)))
              (make-moves
               ,move
               'nothing
               (and (equal nothing-player-position (player position))

                    (eq nothing-player-speed         (player speed))
                    (eq nothing-player-damage        (player damage)))))))))

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
(defmacro states-from (game-state)
  "Produce all possible states using GAME-STATE.

First element of a path is the path taken.
Second is my current position.
Third is my speed.
Fourth is my boosts left.
Fifth is my lizards left.
Sixth is my trucks left.
Seventh is my damage.
Eighth is my boost counter."
  `(bind ((path     '())
          (found    '())
          (explored (make-hash-table :test #'equal)))
     (with-initial-state ,(cons `(iteration (count 3)) game-state)
       (when (null (gethash path explored))
         (if (or (<= (iteration count) 0)
                 (end-state (player position) (game map)))
             (push (list path
                         (player position)
                         (player speed)
                         (player boosts)
                         (player lizards)
                         (player trucks)
                         (player damage)
                         (player boost-counter)
                         (player emps))
                   found)
             (iter
               (for move in (player moves))
               (when (or (eq move 'use_oil)
                         (eq move 'use_emp)
                         (and (member move all-straight-moves)
                              (truck-infront-of (player position) (game map))))
                 (next-iteration))
               (push move path)
               (make-moves
                move
                'nothing
                (recur (1- (iteration count))))
               (setf path (cdr path))))))
     found))

#+nil
(bind ((*ahead-of-cache* (make-hash-table :test #'equal)))
  (states-from ((game (turn 10))
                (iteration (count 3))
                (game-map empty-game-map)
                (player (position (cons 2 1))
                        (boosts 3)
                        (lizards 4)
                        (trucks 5)
                        (speed 6)
                        (damage 0)
                        (boost-counter 8))
                (opponent (position (cons 10 3))
                          (boosts 11)
                          (lizards 12)
                          (trucks 13)
                          (speed 14)
                          (damage 0)
                          (boost-counter 16)))))

(defmacro rank-order-all-moves (game-state)
  "Produce all the moves from GAME-MAP ordered by best placement on the global map.

Given that I'm at MY-POS, ow many BOOSTS, LIZARDS and TRUCKS I have
left, the SPEED at which I'm going and MY-ABS-X position on the
board."
  `(with-initial-state ,game-state
     (bind ((*ahead-of-cache* (make-hash-table :test #'equal)))
      (-> (states-from ,game-state)
        copy-seq
        (sort #'> :key (lambda (state) (bind (((path pos-2 _ boosts-2 lizards-2 trucks-2 damage-2 _ emps-2) state))
                                    (global-score (car pos-2)
                                                  (+ *current-turn* (length path))
                                                  boosts-2
                                                  lizards-2
                                                  trucks-2
                                                  emps-2
                                                  (cdr pos-2)
                                                  damage-2))))))))

(defmacro make-finishing-move (game-state)
  "Optimise for finishing and forget everything else."
  `(bind ((*ahead-of-cache* (make-hash-table :test #'equal)))
     (-> (states-from ,game-state)
       copy-seq
       (sort #'> :key (lambda (state) (nth 2 state)))
       (stable-sort #'< :key (lambda (state) (length (car state))))
       caar
       last
       car)))

(defmacro determine-move (game-state)
  "Produce the best move for GAME-MAP.

Given that I'm at MY-POS, whether I'm BOOSTING, how many BOOSTS,
LIZARDS and TRUCKS I have left, the SPEED at which I'm going and
MY-ABS-X position on the board."
  `(with-initial-state ,game-state
     (bind ((i-am-close-enough-to-opponent (< (opponent x) (+ (player speed) (player x))))
            (opponent-is-behind-me         (> (player   x) (opponent x)))
            (opponent-is-ahead-of-me       (> (opponent x) (player x)))
            (cyber-truck-ahead-of-opponent (and *player-cyber-truck-position*
                                                (> (car *player-cyber-truck-position*)
                                                   (opponent x))))
            (will-crash                    (bind ((*ahead-of-cache* (make-hash-table :test #'equal)))
                                             (> (make-moves
                                                 'nothing
                                                 'nothing
                                                 (player damage))
                                                (player damage))))
            (move (cond
                    ((and i-am-close-enough-to-opponent
                          (not cyber-truck-ahead-of-opponent)
                          (> (player trucks) 0)
                          (not will-crash))
                     (bind ((placement (most-used-square-on-shortest-paths
                                        ,game-state)))
                       (setf *player-cyber-truck-position* placement)
                       (if (null placement)
                           (make-speed-move ,game-state)
                           (cons 'use_tweet placement))))
                    ((opponent-is-close-by (player x)
                                           (cdr (player position))
                                           (opponent x)
                                           (cdr (opponent position)))
                     (bind (((_ _ my-move _ depth) (make-opposed-move ,game-state)))
                       (if (= depth 0) (make-speed-move ,game-state) my-move)))
                    ((close-to-end (player x)) (make-finishing-move ,game-state))
                    (t (make-speed-move ,game-state)))))
       (cond
         ((and (not (and (consp move) (eq (car move) 'USE_TWEET)))
               (no-net-change move ,game-state)
               opponent-is-behind-me
               (> (player oils) 0))
          'use_oil)
         ((and opponent-is-ahead-of-me
               (<= (abs (- (player y) (opponent y))) 1)
               (not (eq *previous-move* 'use_emp))
               (> (player emps) 0)
               (not will-crash))
          'use_emp)
         (t move)))))

(defmacro most-used-square-on-shortest-paths (game-state)
  "Find the shortest path from the opponents current position to me in GAME-STATE."
  `(bind ((square-travel-count (make-hash-table :test #'equal))
          (*ahead-of-cache* (make-hash-table :test #'equal))
          (best-turns (make-array '(1500) :initial-element 0))
          x-limit)
     (with-initial-state ,(cons `(iteration (count 5)) game-state)
       (when (= 5 (iteration count))
         (iter
           (for i from 0 to (opponent x))
           (setf (aref best-turns i) (iteration count)))
         (setf x-limit (min 1499 (+ (player speed) (player x)))))
       (cond
         ((>= (opponent x) (player x)) t)
         ((>= (opponent x) 1500)       t)
         ((= (iteration count) 0)      t)
         (t
          (iter
            (for original-x      = (opponent x))
            (for original-damage = (opponent damage))
            (for opponent-move in (opponent moves))
            ;; The only way to become completely stuck is to not fix
            ;; damage or keep fixing...
            (when (or (and (= 5 (opponent damage))
                           (not (eq opponent-move 'fix)))
                      (and (= 0 (opponent damage))
                           (eq opponent-move 'fix)))
              (next-iteration))
            (make-moves
             'nothing
             opponent-move
             (when (or (> (opponent damage) original-damage)
                       (> (opponent x) x-limit))
               (next-iteration))
             (when (<= (iteration count) 4)
               (accumulate-path (game map)
                                square-travel-count
                                original-x
                                (opponent x)
                                (opponent y)))
             (iter
               (for i from (opponent x) downto 0)
               (while (< (aref best-turns i) (iteration count)))
               (setf (aref best-turns i) (iteration count)))
             (bind ((ahead
                     (iter
                       (for i from (opponent x) below 1500)
                       (while (> (aref best-turns i) (iteration count)))
                       (finally (return (- i (opponent x)))))))
               (when (> (* (iteration count) 15) ahead)
                 (recur (1- (iteration count))))))))))
     (with-open-file (f (format nil "heat-out-~a" *current-turn*)
                        :if-exists :supersede
                        :if-does-not-exist :create
                        :direction :output)
       (format f "~a~%" (iter
                          (with game-map = (make-array '(4 1500) :initial-element 0))
                          (for (key value) in-hashtable square-travel-count)
                          (for (x . y) = key)
                          (setf (aref game-map y x) value)
                          (finally (return (print-full-game-map-to-string (list game-map)))))))
     (with-open-file (f (format nil "map-out-~a" *current-turn*)
                        :if-exists :supersede
                        :if-does-not-exist :create
                        :direction :output)
       (format f "~a~%" (print-full-game-map-to-string (list *full-game-map*))))
     (iter
       (for (key value) in-hashtable square-travel-count)
       (finding key maximizing value))))

(defun print-full-game-map-to-string (full-game-map)
  "Print FULL-GAME-MAP as a string."
  (bind ((output (make-array '(0) :element-type 'extended-char
                                  :fill-pointer 0 :adjustable t))
         (biggest-number
          (iter
            (for y from 0 below 4)
            (maximizing
             (iter
               (for x from 0 below 1500)
               (for value = (aref-game-map full-game-map y x))
               (maximizing (if (numberp value) value 0))) into biggest)
            (finally (return (if (= biggest 0) 1 biggest))))))
    (with-output-to-string (stream output)
      (iter
        (for y from 0 below 4)
        (iter
          (for x from 0 below 1500)
          (format stream "~a" (bind ((tile (aref-game-map full-game-map y x)))
                                (if (numberp tile)
                                    (format nil "~a" (floor (* 9 (/ tile biggest-number))))
                                    (case tile
                                      (mud      #\▓)
                                      (wall     #\#)
                                      (tweet    #\T)
                                      (emp      #\*)
                                      (oil-item #\Φ)
                                      (t        #\░))))))
        (format stream "~%")))
    output))

#+nil
(bind ((*ahead-of-cache* (make-hash-table :test #'equal)))
  (most-used-square-on-shortest-paths
   ((game (turn 10))
    (iteration (count 3))
    (game-map empty-game-map)
    (player (position (cons 2 1))
            (boosts 3)
            (lizards 4)
            (trucks 5)
            (speed 6)
            (damage 0)
            (emps 0)
            (boost-counter 8))
    (opponent (position (cons 10 3))
              (boosts 11)
              (lizards 12)
              (trucks 13)
              (speed 14)
              (damage 0)
              (emps 1)
              (boost-counter 16)))))

(defun accumulate-path (game-map square-travel-count start-x end-x end-y)
  "Increment squares in SQUARE-TRAVEL-COUNT in the given path.

Use relative position of obstacles on GAME-MAP to make certain squares
more highly weighted.

Path starts at (START-X, END-Y) and ends
at (END-X, END-Y)."
  (iter
    (for x from start-x below end-x)
    (for good-squares = (+ (if (is-obstacle-at game-map (1+ end-y) x)      1 0)
                           (if (is-obstacle-at game-map (1- end-y) x)      1 0)
                           (if (is-obstacle-at game-map (1- end-y) (1- x)) 1 0)
                           (if (is-obstacle-at game-map (1+ end-y) (1- x)) 1 0)
                           (if (is-obstacle-at game-map (1+ end-y) (1+ x)) 1 0)
                           (if (is-obstacle-at game-map (1- end-y) (1+ x)) 1 0)))
    (for bad-squares = (+ (if (is-obstacle-at game-map end-y x)      1 0)
                          (if (is-obstacle-at game-map end-y (1+ x)) 1 0)
                          (if (is-obstacle-at game-map end-y (1- x)) 1 0)))
    (incf (gethash (cons x end-y) square-travel-count 0) (1+ (- good-squares bad-squares)))))

(defun is-obstacle-at (game-map y x)
  "Produce t if there's an obstacle at (X, Y) on GAME-MAP."
  (and (< y 4) (> y 0) (> x 0) (< x 1500)
       (member (aref-game-map game-map y x) '(mud wall))))

(defun fill-map (state)
  "Fill *GAME-MAP* with the latest small map from STATE and return it in game-map format."
  (bind ((small-game-map (rows state)))
    (iter
        (for x from 0 below (game-map-x-dim (rows state)))
        (for absolute-x from (max 0 (- (caar (positions state)) 5)) below 1500)
        (iter
          (for y from 0 below 4)
          (setf (aref *full-game-map* y absolute-x)
                (aref-game-map small-game-map y x))))
    (cons *full-game-map*
          (cdr small-game-map))))

(defun move-for-state (state)
  "Produce the move which my bot makes from STATE."
  (bind (((player-position . opponent-position) (positions state))

         (player-boosts            (my-boosts state))
         (player-boost-counter     (my-boost-counter state))
         (player-lizards           (my-lizards state))
         (player-trucks            (my-trucks state))
         (player-emps              (my-emps state))
         (player-oils              (my-oils state))
         (player-speed             (my-speed state))
         (player-damage            (my-damage state))
         (opponent-boosts          1)
         (opponent-emps            0) ; (TODO decide what's best here) Start off, assuming that he has none.
         (opponent-speed           (opponent-speed state))

         (filled-game-map          (fill-map state))

         (move                     (determine-move ((game (turn *current-turn*))
                                                    (game-map filled-game-map)
                                                    (player
                                                     (position player-position)
                                                     (boosts player-boosts)
                                                     (oils player-oils)
                                                     (lizards player-lizards)
                                                     (trucks player-trucks)
                                                     (emps player-emps)
                                                     (speed player-speed)
                                                     (damage player-damage)
                                                     (boost-counter player-boost-counter))
                                                    (opponent
                                                     (position opponent-position)
                                                     (boosts opponent-boosts)
                                                     (oils 0)
                                                     (lizards 1)
                                                     (trucks 1)
                                                     (emps opponent-emps)
                                                     (speed opponent-speed)
                                                     (damage 0)
                                                     (boost-counter 0))))))
    (setf *previous-move* move)
    (format *error-output*
            "My total/average speed: ~a - ~a~%"
            (car player-position)
            (/ (car player-position) *current-turn*))
    (format *error-output*
            "Op total/average speed: ~a - ~a~%"
            (car opponent-position)
            (/ (car opponent-position) *current-turn*))
    move))

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
  "Produce the number of lizards which I have in THIS state."
  (count-if (lambda (x) (string-equal x "LIZARD"))
            (deep-accessor this 'player 'powerups)))

(defmethod my-trucks ((this state))
  "Produce the number of trucks which I have in THIS state."
  (count-if (lambda (x) (string-equal x "TWEET"))
            (deep-accessor this 'player 'powerups)))

(defmethod my-emps ((this state))
  "Produce the number of emps which I have in THIS state."
  (count-if (lambda (x) (string-equal x "EMP"))
            (deep-accessor this 'player 'powerups)))

(defmethod my-oils ((this state))
  "Produce the number of oils which I have in THIS state."
  (count-if (lambda (x) (string-equal x "OIL"))
            (deep-accessor this 'player 'powerups)))

(defmethod my-boost-counter ((this state))
  "Produce the number of boosts which I have in THIS state."
  (deep-accessor this 'player 'boost-counter))

(defmethod my-speed ((this state))
  "Produce the speed which I'm going in THIS state."
  (deep-accessor this 'player 'player-speed))

(defmethod my-damage ((this state))
  "Produce the damage which I've sustained in THIS state."
  (deep-accessor this 'player 'damage))

(defmethod opponent-speed ((this state))
  "Produce the speed which the opponent is going in THIS state."
  (deep-accessor this 'opponent 'player-speed))

(defun close-to-end (x)
  "Produce T if X is close to the edge of the map."
  (> x 1480))

(defmacro cannot-make-move (boosts oils lizards trucks pos emps)
  "Produce a function which produces T if MOVE can't be made with BOOSTS, LIZARDS, TRUCKS and EMPS from POS."
  `(lambda (move) (not (move-can-be-made move ,boosts ,oils ,lizards ,trucks (cdr ,pos) ,emps))))

(defun remove-impossible-moves (boosts oils lizards trucks pos emps all-makeable-moves)
  "Remove impossible moves from ALL-MOVES.

Given that the player has BOOSTS, LIZARDS and TRUCKS left and is at
POS."
  (remove-if (cannot-make-move boosts oils lizards trucks pos emps) all-makeable-moves))

;; TODO: Include oils...
(defun global-score (x current-turn boosts lizards trucks emps y damage)
  "Score the position described by X BOOSTS LIZARDS."
  (bind ((is-middle-two (if (or (= y 1)
                                (= y 2))
                            1
                            0)))
    (iter (for coefficients in *heuristic-coeficients*)
      (bind (((x-score
               boosts-score
               lizards-score
               trucks-score
               emp-score
               y-score
               damage-score
               current-turn-score) coefficients))
        (maximizing (+ (* x-score             x)
                       (* boosts-score        boosts)
                       (* lizards-score       lizards)
                       (* trucks-score        trucks)
                       (* emp-score           emps)
                       (* y-score             is-middle-two)
                       (* damage-score        damage)
                       (* current-turn-score  current-turn)))))))

(defun minimax-score (my-abs-x op-abs-x)
  "Compute a score me on MY-ABS-X and the opponent is on OP-ABS-X."
  (- my-abs-x op-abs-x))

(defun game-map-y-dim (game-map)
  "Produce the number of squares in the y dimension of GAME-MAP."
  (array-dimension (car game-map) 0))

(defun game-map-x-dim (game-map)
  "Produce the number of squares in the x dimension of GAME-MAP."
  (array-dimension (car game-map) 1))

(defconstant window-ahead-to-consider-maximax 15
  "The window ahead me that I should use to consider using maximax.")

(defconstant window-behind-to-consider-maximax 5
  "The window behind me that I should use to consider using maximax.")

(defun opponent-is-close-by (my-abs-x my-y opponent-abs-x opponent-y)
  "Produce t if MY-ABS-X is at a position where I can see OPPONENT-ABS-X."
  (and (>= opponent-abs-x (- my-abs-x window-behind-to-consider-maximax))
       (<= opponent-abs-x (+ my-abs-x window-ahead-to-consider-maximax))
       (>= opponent-y     (- my-y     1))
       (<= opponent-y     (+ my-y     1))))

(defun truck-infront-of (current-pos game-map)
  "Produce t if there is a truck immediately in front of CURRENT-POS on GAME-MAP."
  (bind (((_ . trucks) game-map)
         ((x . y)      current-pos))
    (iter
      (for (x-truck . y-truck) in trucks)
      (thereis (and (eq y-truck y)
                    (eq x (1- x-truck)))))))

(defmacro ahead-of (collision-result move game-map start-position end-position)
  "Produce the appropriate `ahead-of' form.

Take into account the nuances of changing direction and not counting
the current square.  e.g. changing direction means that you travel
diagonally ignoring the squars on the rectalinear path.

MOVE can modify where we look.  For instance a MOVE of LIZARD means
that we need to only look at the square which we land on.

If TYPE is 'SPEED then produce `boost-ahead-of' if it's 'MUD then
produce `mud-ahead-of' etc.

SPEED, GAME-MAP, and POS should be un-adjusted values."
  (bind ((direction `(case ,move
                       (turn_left  'up)
                       (turn_right 'down)
                       (otherwise  'ahead)))
         (adj-speed `(case ,move
                       (use_lizard 0)
                       (fix        0)
                       (t (+ (- (car ,end-position)
                                (car ,start-position))
                             (if (or (eq ,direction 'ahead)
                                     (eq ,collision-result 'side-collision))
                                 -1
                                 0)))))
         (adj-x     `(if (eq ,move 'use_lizard)
                         (car ,end-position)
                         (if (eq ,move 'fix)
                             (car ,start-position)
                             (ecase ,direction
                               (up    (+ (car ,start-position)
                                         (if (eq ,collision-result 'side-collision) 1 0)))
                               (down  (+ (car ,start-position)
                                         (if (eq ,collision-result 'side-collision) 1 0)))
                               (ahead (1+ (car ,start-position)))))))
         (adj-y     `(cdr ,end-position)))
    `(if (eq ,move 'fix) (vector 0 0 0 0 0 0 0)
         (or (gethash (list ,adj-speed ,adj-x ,adj-y) *ahead-of-cache*)
             (setf (gethash (list ,adj-speed ,adj-x ,adj-y) *ahead-of-cache*)
                   (all-ahead-of ,adj-speed ,game-map ,adj-x ,adj-y))))))

(defun all-ahead-of (speed game-map x y)
  "Produce a count of all types of powerups and obstacles.

When going at SPEED from X, Y on GAME-MAP."
  (iter
    (for i from (max x 0) below (min (1+ (+ x speed)) (game-map-x-dim game-map)))
    (for tile = (aref-game-map game-map y i))
    (counting (eq 'mud tile)      into muds)
    (counting (eq 'wall tile)     into walls)
    (counting (eq 'boost tile)    into boosts)
    (counting (eq 'lizard tile)   into lizards)
    (counting (eq 'tweet tile)    into tweets)
    (counting (eq 'emp tile)      into emps)
    (counting (eq 'oil-item tile) into oils)
    (finally (return (vector muds boosts walls tweets lizards emps oils)))))

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
  (defun maximum-speed (damage)
    "Produce the maximum speed that your car can go at when at DAMAGE level."
    (case damage
      (0 15)
      (1 9)
      (2 8)
      (3 6)
      (4 5)
      (5 3)
      (t 0)))

  (defun increase-speed (speed damage)
    "Produce the speed which is one faster than SPEED.

Limit the maximum speed by DAMAGE."
    (min (case speed
           (0 3)
           (3 6)
           (5 6)
           (6 8)
           (8 9)
           (t speed))
         (maximum-speed damage)))

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

(defmacro new-speed (move speed damage)
  "Produce the speed which MOVE will change SPEED to.

Limit the maximum speed by the amount of damage taken."
  `(case ,move
     (accelerate (increase-speed ,speed ,damage))
     (decelerate (decrease-speed ,speed))
     (use_boost  (min 15 (maximum-speed ,damage)))
     (otherwise  ,speed)))

(defmacro new-x (x move speed damage)
  "Produce the new value of X when MOVE is made at SPEED."
  `(if (or (= 0 (maximum-speed ,damage))
           (= 0 ,speed))
       ,x
       (case ,move
         (turn_left  (move-car up    ,speed ,x))
         (turn_right (move-car down  ,speed ,x))
         (fix        ,x)
         (otherwise  (move-car ahead ,speed ,x)))))

(defmacro new-y (y move speed damage)
  "Produce the new value of Y when MOVE is made."
  `(if (or (= 0 (maximum-speed ,damage))
           (= 0 ,speed))
       ,y
       (case ,move
         (turn_left  (move-lat up    ,y))
         (turn_right (move-lat down  ,y))
         (otherwise  (move-lat ahead ,y)))))

(defun manual-decelerate (speed)
  "Decelerate from SPEED as an action."
  (if (= speed 3)
      0
      (decrease-speed speed)))

;; TODO: deal with collision state in:
;; "../EntelectChallenge-2020-Overdrive/game-runner/match-logs/2020.08.08.12.33.49"
(defun stage-positions (move other-move position other-position speed damage boost-counter)
  "Make MOVE across from POSITION at SPEED with BOOST-COUNTER

Produces staged values of position speed and the new boost counter."
  (if (and (eq other-move 'use_emp)
           (<= (abs (- (cdr position) (cdr other-position))) 1)
           (< (car other-position) (car position)))
      (values position 3 0 'was-empd)
      (bind ((new-speed         (new-speed move
                                           (if (= 1 boost-counter)
                                               (min (maximum-speed damage) 9)
                                               speed)
                                           damage))
             ((x . y)           position)
             (new-x             (new-x x move new-speed damage))
             (new-y             (new-y y move new-speed damage))
             (new-pos           (cons new-x new-y))
             (new-boost-counter (if (eq move 'decelerate) 0 boost-counter)))
        (values new-pos new-speed new-boost-counter 'not-empd))))

(defun resolve-collisions (one-start other-start one-end other-end move other-move game-map damage speed boost-counter empd)
  "Resolve collisions and produce the new position, damage and speed of one car."
  (bind (((one-start-x   . one-start-y)   one-start)
         ((other-start-x . other-start-y) other-start)
         ((one-end-x     . one-end-y)     one-end)
         ((other-end-x   . other-end-y)   other-end)
         (one-got-ahead                   (and (>= one-end-x other-end-x)
                                               (< one-start-x other-start-x)))
         (start-lane-same                 (= one-start-y other-start-y))
         (end-lane-same                   (= one-end-y other-end-y))
         (side-collision                  (and (not start-lane-same)
                                               (equal one-end other-end)))
         (truck-x                         (hit-a-truck game-map one-start-x one-end-x one-start-y one-end-y))
         (one-hit-other-back              (and (not (eq other-move 'use_lizard))
                                               one-got-ahead
                                               start-lane-same
                                               end-lane-same))
         (one-hit-truck-before-other      (and truck-x (or (not (or one-hit-other-back side-collision))
                                                           (< truck-x other-end-x))))
         (boost-counter-2                 (if (and (eq empd 'not-empd)
                                                   (eq move 'use_boost))
                                              5
                                              (max 0 (1- boost-counter)))))
    (cond
      (one-hit-truck-before-other (values 'hit-truck
                                          (cons (1- truck-x) one-end-y)
                                          (+ damage 2)
                                          3
                                          0))
      (one-hit-other-back         (values 'hit-back
                                          (cons (1- other-end-x) one-end-y)
                                          damage
                                          speed
                                          boost-counter-2))
      (side-collision             (values 'side-collision
                                          (cons (1- one-end-x) one-start-y)
                                          damage
                                          speed
                                          boost-counter-2))
      (t                          (values 'no-collision
                                          one-end
                                          damage
                                          speed
                                          boost-counter-2)))))

(defun interact-with-map (collision-result
                          start-position
                          end-position
                          move
                          other-move
                          other-position
                          game-map
                          damage
                          speed
                          boosts
                          oils
                          lizards
                          trucks
                          emps
                          boost-counter)
  "Interact with obstacles and powerups on the GAME-MAP.

Start from START-POSITION and end at END-POSITION, accumulating into
DAMAGE, SPEED, BOOSTS, LIZARDS and TRUCKS."
  (bind (((oil-x . oil-y) other-position)
         (using-oil       (and (eq other-move 'use_oil)
                               (< (car other-position) (game-map-x-dim game-map))))
         (original-tile   (when using-oil (aref-game-map game-map oil-y oil-x))))
    (when using-oil (setf-game-map game-map oil-y oil-x 'mud))
    (bind ((all-hit           (ahead-of collision-result
                                        move
                                        game-map
                                        start-position
                                        end-position))
           (muds-hit          (aref all-hit 0))
           (walls-hit         (aref all-hit 2))
           (new-boosts        (+ (if (eq move 'use_boost) -1 0)
                                 (aref all-hit 1)
                                 boosts))
           (new-lizards       (+ (if (eq move 'use_lizard) -1 0)
                                 (aref all-hit 4)
                                 lizards))
           (new-trucks        (+ (if (and (consp move) (eq (car move) 'use_tweet)) -1 0)
                                 (aref all-hit 3)
                                 trucks))
           (new-emps          (+ (if (eq move 'use_emp) -1 0)
                                 (aref all-hit 5)
                                 emps))
           (new-oils          (+ (if (eq move 'use_oil) -1 0)
                                 (aref all-hit 6)
                                 oils))
           (new-damage        (min 6 (+ muds-hit
                                        (* 2 walls-hit)
                                        (max 0 (if (eq move 'fix) (- damage 2) damage)))))
           (final-speed       (min (maximum-speed new-damage)
                                   (if (> walls-hit 0)
                                       (min speed 3)
                                       (decrease-speed-by muds-hit speed))))
           (boost-counter-2   (if (or (> muds-hit 0)
                                      (> walls-hit 0))
                                  0
                                  boost-counter)))
      (when using-oil (setf-game-map game-map oil-y oil-x original-tile))
      (values new-boosts new-oils final-speed new-lizards new-trucks new-emps new-damage boost-counter-2))))

(defun hit-a-truck (game-map start-x end-x start-y new-y)
  "Produce t if you would hit a truck on GAME-MAP from START-X.

NEW-Y is the lane which the car ends up in, and END-X is where you end
up in, in your lane."
  (bind (((_ . trucks) game-map))
    (iter
      (for (x-truck . y-truck) in trucks)
      (when (and (= y-truck new-y)
                 (<= start-x x-truck)
                 (not (and (= start-x x-truck)
                           (= start-y y-truck)))
                 (>= end-x x-truck))
        (collecting x-truck into hits))
      (finally (return (iter (for x in hits)
                         (minimize x)))))))

(defun decrease-speed-by (times speed)
  "Reduce SPEED TIMES times."
  (iter
    (with final-speed = speed)
    (for i from 0 below times)
    (setf final-speed (decrease-speed final-speed))
    (finally (return final-speed))))

(defun end-state (position game-map)
  "Produce T if POSITION, is considered an end state for the search in GAME-MAP.

Being at the edge of the map is best, because we don't know what's
coming!"
  (>= (car position) (1- (game-map-x-dim game-map))))

(defun move-can-be-made (move boosts oils lizards trucks y emps)
  "Produce T if the MOVE can be made from Y coordinate."
  (case move
    (turn_right (< y 3))
    (turn_left  (> y 0))
    (use_boost  (> boosts 0))
    (use_tweet  (> trucks 0))
    (use_lizard (> lizards 0))
    (use_emp    (> emps 0))
    (use_oil    (> oils 0))
    (t          t)))

(defun aref-game-map (game-map y x)
  "Produce the value in GAME-MAP at coordinate Y, X."
  (aref (car game-map) y x))

(defun setf-game-map (game-map y x value)
  "Set the VALUE of GAME-MAP at coordinate Y, X."
  (setf (aref (car game-map) y x) value))

(defun aref-game-map-with-default (game-map y x &optional default)
  "Produce the value in GAME-MAP at coordinate Y, X.

If X, Y is OOB then produce DEFAULT."
  (if (end-state (cons x y) game-map)
      default
      (aref (car game-map) y x)))

(defun load-state-file (round)
  "Load the state file for ROUND."
  (load-state-from-file (format nil "rounds/~a/state.json" round)))

(defun load-state-from-file (file-path)
  "Load the state from file at FILE-PATH."
  (with-open-file (f file-path)
    (parse-state f)))

(defmethod minimum-x ((this state))
  "Produce the minimum value of x in the map in THIS state."
  (1- (deep-accessor (aref (aref (slot-value this 'world-map) 0) 0)
                     'map-position 'x)))

(defmethod rows ((this state))
  "Produce rows as a 2D array of cells from the map in THIS state."
  (iter
    (with world-map = (slot-value this 'world-map))
    (with trucks = '())
    (with result = (make-array (list (length world-map)
                                     (length (aref world-map 0)))
                               :initial-element nil))
    (with x-start = (minimum-x this))
    (for y from 0)
    (for row in-vector world-map)
    (iter
      (for cell in-vector row)
      (for x from 0)
      (when (is-occupied-by-cyber-truck cell)
        (push (cons (+ x-start x) y) trucks))
      (setf (aref result y x)
            (case (slot-value cell 'surface-object)
              (0 nil)
              (1 'mud)
              (2 'mud) ; Would have been an oil spill, but there's no difference(!)
              (3 'oil-item)
              (4 'finish-line)
              (5 'boost)
              (6 'wall)
              (7 'lizard)
              (8 'tweet)
              (9 'emp))))
    (finally (return (cons result trucks)))))

(defmethod position-to-cons ((this map-position))
  "Produce a cons cell of postions from THIS position."
  (cons (slot-value this 'x)
        (slot-value this 'y)))

(defmethod positions ((this state))
  "Produce the player positions in THIS state."
  (cons (to-zero-indexed (position-to-cons (deep-accessor this 'player   'map-position)))
        (to-zero-indexed (position-to-cons (deep-accessor this 'opponent 'map-position)))))

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
                             (remove-if (lambda (path) (ppcre:all-matches (concatenate 'string
                                                                                       "- "
                                                                                       ,player
                                                                                       "/")
                                                                          path)))
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
    (bind ((move-parts (->> (cl-ppcre:split "Command: " (read-line f))
                         cadr
                         (cl-ppcre:split " "))))
      (if (> (length move-parts) 1)
          (cons (read-from-string (car move-parts))
                (cons (read-from-string (cadr move-parts))
                      (read-from-string (caddr move-parts))))
          (read-from-string (car move-parts))))))

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

;; (defun compare-rankings-to-opponent-move (folder-path opponent-name opponent-player)
;;   "Compare my decisions to the opponents for each round in FOLDER-PATH."
;;   (with-consecutive-states folder-path opponent-name opponent-player
;;     (declare (ignore opponent-move next-state))
;;     (format t "========================================~%")
;;     (format t "==============Round: ~a=============~%~%" round)
;;     (bind ((*ahead-of-cache*   (make-hash-table :test #'equal))
;;            (game-map           (rows current-state))
;;            ((my-pos . op-pos)  (positions current-state))
;;            (my-abs-x           (my-abs-x current-state))
;;            (opponent-abs-x     (opponent-abs-x current-state))
;;            (boosting           (im-boosting current-state))
;;            (boosts             (my-boosts current-state))
;;            (lizards            (my-lizards current-state))
;;            (trucks             (my-trucks current-state))
;;            (speed              (my-speed current-state))
;;            (damage             (my-damage current-state))
;;            (boost-counter      (my-boost-counter current-state))
;;            (op-boosts          1)
;;            (op-speed           (opponent-speed current-state))
;;            (move-i-would-make  (determine-move game-map
;;                                                my-pos
;;                                                boosting
;;                                                boosts
;;                                                lizards
;;                                                trucks
;;                                                speed
;;                                                damage
;;                                                boost-counter
;;                                                my-abs-x
;;                                                opponent-abs-x
;;                                                op-pos
;;                                                op-boosts
;;                                                op-speed))
;;            (ranked-speed-moves (rank-order-all-moves ((game (turn *current-turn*))
;;                                                       (game-map game-map)
;;                                                       (player (absolute-x my-abs-x)
;;                                                               (position my-pos)
;;                                                               (boosts boosts)
;;                                                               (lizards lizards)
;;                                                               (trucks trucks)
;;                                                               (speed speed)
;;                                                               (damage damage)
;;                                                               (boost-counter boost-counter))
;;                                                       (opponent (absolute-x opponent-abs-x)
;;                                                                 (position op-pos)
;;                                                                 (boosts op-boosts)
;;                                                                 (lizards 1)
;;                                                                 (trucks 1)
;;                                                                 (speed 1)
;;                                                                 (damage 0)
;;                                                                 (boost-counter 0))))))
;;       (format t "I'm making a ~a move.~%"
;;               (cond
;;                 ((and (> trucks 0)
;;                       (> op-speed 3)
;;                       (opponent-is-close-by my-abs-x (cdr my-pos) opponent-abs-x (cdr op-pos)))
;;                  'cyber)
;;                 ((opponent-is-close-by my-abs-x (cdr my-pos) opponent-abs-x (cdr op-pos))
;;                  'opposed)
;;                 (t 'speed)))
;;       (format t "I would make:  ~a~%"   move-i-would-make)
;;       (format t "Op made:       ~a~%~%" current-move)
;;       (format t
;;               "How I rank speed moves:~% ~{~a~^~%~}~%"
;;               ranked-speed-moves)
;;       (format t "========================================~%"))))

(defun process-moves (player-position
                      player-boosts
                      player-oils
                      player-lizards
                      player-trucks
                      player-emps
                      player-speed
                      player-damage
                      player-boost-counter

                      opponent-position
                      opponent-boosts
                      opponent-oils
                      opponent-lizards
                      opponent-trucks
                      opponent-emps
                      opponent-speed
                      opponent-damage
                      opponent-boost-counter

                      current-game-map

                      player-move
                      opponent-move)
  "Process GAME-STATE, producing new values for the player and opponent state.

Where the players make PLAYER-MOVE and OPPONENT-MOVE respectively."
  (bind (((:values player-position-2-staged
                   player-speed-staged
                   player-decelerate-boost-counter
                   player-empd)
          (stage-positions player-move
                           opponent-move
                           player-position
                           opponent-position
                           player-speed
                           player-damage
                           player-boost-counter))
         ((:values opponent-position-2-staged
                   opponent-speed-staged
                   opponent-decelerate-boost-counter
                   opponent-empd)
          (stage-positions opponent-move
                           player-move
                           opponent-position
                           player-position
                           opponent-speed
                           opponent-damage
                           opponent-boost-counter))
         ((:values player-collision-result
                   player-position-2
                   player-truck-damage
                   player-truck-speed
                   player-truck-boost-counter)
          (resolve-collisions player-position
                              opponent-position
                              player-position-2-staged
                              opponent-position-2-staged
                              player-move
                              opponent-move
                              current-game-map
                              player-damage
                              player-speed-staged
                              player-decelerate-boost-counter
                              player-empd))
         ((:values opponent-collision-result
                   opponent-position-2
                   opponent-truck-damage
                   opponent-truck-speed
                   opponent-truck-boost-counter)
          (resolve-collisions opponent-position
                              player-position
                              opponent-position-2-staged
                              player-position-2-staged
                              opponent-move
                              player-move
                              current-game-map
                              opponent-damage
                              opponent-speed-staged
                              opponent-decelerate-boost-counter
                              opponent-empd))
         ((:values player-boosts-2
                   player-oils-2
                   player-speed-2
                   player-lizards-2
                   player-trucks-2
                   player-emps-2
                   player-damage-2
                   player-boost-counter-2)
          (interact-with-map player-collision-result
                             player-position
                             player-position-2
                             player-move
                             opponent-move
                             opponent-position
                             current-game-map
                             player-truck-damage
                             player-truck-speed
                             player-boosts
                             player-oils
                             player-lizards
                             player-trucks
                             player-emps
                             player-truck-boost-counter))
         ((:values opponent-boosts-2
                   opponent-oils-2
                   opponent-speed-2
                   opponent-lizards-2
                   opponent-trucks-2
                   opponent-emps-2
                   opponent-damage-2
                   opponent-boost-counter-2)
          (interact-with-map opponent-collision-result
                             opponent-position
                             opponent-position-2
                             opponent-move
                             player-move
                             player-position
                             current-game-map
                             opponent-truck-damage
                             opponent-truck-speed
                             opponent-boosts
                             opponent-oils
                             opponent-lizards
                             opponent-trucks
                             opponent-emps
                             opponent-truck-boost-counter)))
    (values player-position-2
            player-boosts-2
            player-oils-2
            player-lizards-2
            player-trucks-2
            player-emps-2
            (if (eq player-collision-result 'side-collision) (manual-decelerate player-speed-2) player-speed-2)
            player-damage-2
            player-boost-counter-2
            opponent-position-2
            opponent-boosts-2
            opponent-oils-2
            opponent-lizards-2
            opponent-trucks-2
            opponent-emps-2
            (if (eq opponent-collision-result 'side-collision) (manual-decelerate opponent-speed-2) opponent-speed-2)
            opponent-damage-2
            opponent-boost-counter-2)))

(defun replay-from-folder (folder-path &rest rounds)
  "Check that `make-move' produces the same result as the target engine."
  (bind ((*full-game-map* (make-array '(4 1500) :initial-element nil)))
    (with-consecutive-states folder-path "Quantum" 'A
     (when (or (null rounds)
               (and rounds
                    (member round rounds)))
       (bind ((*ahead-of-cache* (make-hash-table :test #'equal))
              (filled-game-map  (fill-map current-state))
              (player-move      current-move)

              ((player-position . opponent-position) (positions current-state))

              (player-speed         (my-speed current-state))
              (player-boosts        (my-boosts current-state))
              (player-oils          (my-oils current-state))
              (player-lizards       (my-lizards current-state))
              (player-trucks        (my-trucks current-state))
              (player-emps          (my-emps   current-state))
              (player-damage        (my-damage current-state))
              (player-boost-counter (my-boost-counter current-state))

              (opponent-speed         (opponent-speed current-state))
              (opponent-boosts        0)
              (opponent-oils          0)
              (opponent-lizards       0)
              (opponent-trucks        0)
              (opponent-emps          0)
              (opponent-damage        0)
              (opponent-boost-counter 0)

              ((:values player-position-2
                        player-boosts-2
                        player-oils-2
                        player-lizards-2
                        player-trucks-2
                        player-emps-2
                        player-speed-2
                        player-damage-2
                        player-boost-counter-2

                        opponent-position-2
                        opponent-boosts-2
                        opponent-oils-2
                        opponent-lizards-2
                        opponent-trucks-2
                        opponent-emps-2
                        opponent-speed-2
                        opponent-damage-2
                        opponent-boost-counter-2)
               (process-moves player-position
                              player-boosts
                              player-oils
                              player-lizards
                              player-trucks
                              player-emps
                              player-speed
                              player-damage
                              player-boost-counter

                              opponent-position
                              opponent-boosts
                              opponent-oils
                              opponent-lizards
                              opponent-trucks
                              opponent-emps
                              opponent-speed
                              opponent-damage
                              opponent-boost-counter

                              filled-game-map

                              player-move
                              opponent-move))
              (initial    (list (my-abs-pos       current-state)
                                (my-speed         current-state)
                                (my-boosts        current-state)
                                (my-oils          current-state)
                                (my-lizards       current-state)
                                (my-trucks        current-state)
                                (my-emps          current-state)
                                (my-damage        current-state)
                                (my-boost-counter current-state)))
              (computed   (list player-position-2
                                player-speed-2
                                player-boosts-2
                                player-oils-2
                                player-lizards-2
                                player-trucks-2
                                player-emps-2
                                player-damage-2
                                player-boost-counter-2))
              (actual     (list (my-abs-pos       next-state)
                                (my-speed         next-state)
                                (my-boosts        next-state)
                                (my-oils          next-state)
                                (my-lizards       next-state)
                                (my-trucks        next-state)
                                (my-emps          next-state)
                                (my-damage        next-state)
                                (my-boost-counter next-state))))
         (when (not (equal computed actual))
           (format t "~s:~6T~a~%~a~%~6T~a~%~6T~a~%========================================~%"
                   round
                   initial
                   current-move
                   computed
                   actual)))))))

#+nil
(progn
  (require :sb-sprof)
  (defun profile-a-game (folder-path)
    "Run the profiler on my bot for each round in FOLDER-PATH."
    (sb-sprof:with-profiling (:max-samples 1000
                              :report :flat
                              :loop t)
      (format t "Tick~%")
      (with-consecutive-states folder-path "Quantum" 'A
        (declare (ignore current-move opponent-move next-state))
        (move-for-state current-state)))))
