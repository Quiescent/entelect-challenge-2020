(in-package :bot)

(defvar *heuristic-coeficients* '((1 1 1 1 1 1))
  "The coefficients to use when computing the score of a position.")

(defvar *current-turn* 1
  "The turn of the game that we're on.")

(defvar *previous-move* nil
  "The move which was made last turn.")

(defun read-weights ()
  "Read all scores from the score config file."
  (with-open-file (f "./score-config")
    (iter
      (for next-member = (eval (ignore-errors (read f))))
      (while next-member)
      (collecting next-member))))

(defun main ()
  (iter
    (while t)
    (initially (setf *heuristic-coeficients* (read-weights)))
    (for round-number = (read-line))
    (bind ((*current-turn* (read-from-string round-number)))
      (for move = (move-for-round round-number)))
    (format t "C;~a;~a~%" round-number (move-to-string move))))

(defun move-to-string (move)
  "Produce a string representation of MOVE."
  (if (consp move)
      (format nil "~a ~a ~a" (car move) (cddr move) (cadr move))
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

(defvar all-makeable-moves '(accelerate use_boost turn_right turn_left nothing decelerate use_lizard fix)
  "All the moves which I can make.")

(defvar all-straight-moves '(accelerate use_boost nothing decelerate)
  "All moves which will result in going straight without jumping.")

;; Might want to try turning on optimsations for this to get to depth
;; three!
(defconstant maximax-depth 2
  "The depth that we should search the game tree.")

;; The moves being made here don't make sense!
(defmacro make-opposed-move (game-state)
  "Produce the best move in GAME-STATE as determined by a few rounds of maximax."
  `(bind ((*ahead-of-cache* (make-hash-table :test #'equal)))
     (with-initial-state ,(cons `(iteration (count ,maximax-depth)) game-state)
       (iter
         (for current-turn = (+ *current-turn* (- maximax-depth (iteration count))))
         (with cells =
               (iter
                 (for player-move in (player moves))
                 (collecting
                  (iter
                    (for opponent-move in (opponent moves))
                    (make-moves
                     player-move
                     opponent-move
                     (bind (((player-score opponent-score _ _ _)
                             (if (or (end-state (player position)   game-map)
                                     (end-state (opponent position) game-map)
                                     (< (iteration count) 1))
                                 (list (player score) (opponent score) nil nil nil)
                                 (recur (1- (iteration count))))))
                       (finding (list player-score opponent-score player-move opponent-move (- maximax-depth (iteration count)))
                                maximizing opponent-score)))))))
         ;; (initially
         ;;  (when (eq maximax-depth (iteration count)) (format t "~a: ~{ ~a ~%~}~%"
         ;;                                    (iteration count)
         ;;                                    (mapcar (lambda (cell) (cons (coerce (car cell) 'float) (cons (coerce (cadr cell) 'float) (cddr cell)))) cells))))
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
        (player-not-defined 'absolute-x)
        (player-not-defined 'position)
        (player-not-defined 'boosts)
        (player-not-defined 'lizards)
        (player-not-defined 'trucks)
        (player-not-defined 'emps)
        (player-not-defined 'speed)
        (player-not-defined 'damage)
        (player-not-defined 'boost-counter)

        (opponent-not-defined 'absolute-x)
        (opponent-not-defined 'position)
        (opponent-not-defined 'boosts)
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

                (player-absolute-x    ,@(cdr (assoc 'absolute-x    player-state)))
                (player-position      ,@(cdr (assoc 'position      player-state)))
                (player-boosts        ,@(cdr (assoc 'boosts        player-state)))
                (player-lizards       ,@(cdr (assoc 'lizards       player-state)))
                (player-trucks        ,@(cdr (assoc 'trucks        player-state)))
                (player-emps          ,@(cdr (assoc 'emps          player-state)))
                (player-speed         ,@(cdr (assoc 'speed         player-state)))
                (player-damage        ,@(cdr (assoc 'damage        player-state)))
                (player-boost-counter ,@(cdr (assoc 'boost-counter player-state)))

                (opponent-absolute-x    ,@(cdr (assoc 'absolute-x    opponent-state)))
                (opponent-position      ,@(cdr (assoc 'position      opponent-state)))
                (opponent-boosts        ,@(cdr (assoc 'boosts        opponent-state)))
                (opponent-lizards       ,@(cdr (assoc 'lizards       opponent-state)))
                (opponent-trucks        ,@(cdr (assoc 'trucks        opponent-state)))
                (opponent-emps          ,@(cdr (assoc 'emps          opponent-state)))
                (opponent-speed         ,@(cdr (assoc 'speed         opponent-state)))
                (opponent-damage        ,@(cdr (assoc 'damage        opponent-state)))
                (opponent-boost-counter ,@(cdr (assoc 'boost-counter opponent-state)))

                (game-turn              ,@(cdr (assoc 'turn game-state)))

                (iteration-count        ,@(and iteration-state (cdr (assoc 'count iteration-state)))))
           (macrolet ((make-moves (player-move opponent-move &rest subsequent)
                        `(bind (((:values player-position-2-staged
                                          player-speed-2
                                          player-boosts-2
                                          player-lizards-2
                                          player-trucks-2
                                          player-emps-2
                                          player-damage-2
                                          player-boost-counter-2)
                                 (make-move ,player-move
                                            ,opponent-move
                                            current-game-map
                                            opponent-position
                                            player-position
                                            player-speed
                                            player-boosts
                                            player-lizards
                                            player-trucks
                                            player-emps
                                            player-damage
                                            player-boost-counter))
                                ((:values opponent-position-2-staged
                                          opponent-speed-2
                                          opponent-boosts-2
                                          opponent-lizards-2
                                          opponent-trucks-2
                                          opponent-emps-2
                                          opponent-damage-2
                                          opponent-boost-counter-2)
                                 (make-move ,opponent-move
                                            ,player-move
                                            current-game-map
                                            player-position
                                            opponent-position
                                            opponent-speed
                                            opponent-boosts
                                            opponent-lizards
                                            opponent-trucks
                                            opponent-emps
                                            opponent-damage
                                            opponent-boost-counter))
                                (player-position-2 (resolve-collisions player-position
                                                                       opponent-position
                                                                       player-position-2-staged
                                                                       opponent-position-2-staged))
                                (opponent-position-2 (resolve-collisions opponent-position
                                                                         player-position
                                                                         opponent-position-2-staged
                                                                         player-position-2-staged))
                                (opponent-absolute-x-2 (+ (- (car opponent-position-2) (car opponent-position))
                                                          opponent-absolute-x))
                                (player-absolute-x-2 (+ (- (car player-position-2) (car player-position))
                                                        player-absolute-x)))
                           (bind ((current-game-map current-game-map)

                                  (player-absolute-x    player-absolute-x-2)
                                  (player-position      player-position-2)
                                  (player-boosts        player-boosts-2)
                                  (player-lizards       player-lizards-2)
                                  (player-trucks        player-trucks-2)
                                  (player-emps          player-emps-2)
                                  (player-speed         player-speed-2)
                                  (player-damage        player-damage-2)
                                  (player-boost-counter player-boost-counter-2)

                                  (opponent-absolute-x    opponent-absolute-x-2)
                                  (opponent-position      opponent-position-2)
                                  (opponent-boosts        opponent-boosts-2)
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
                                                                opponent-absolute-x
                                                                game-turn
                                                                opponent-boosts
                                                                opponent-lizards
                                                                (cdr opponent-position)
                                                                opponent-damage))
                                                       (moves '(remove-impossible-moves opponent-boosts
                                                                opponent-lizards
                                                                opponent-trucks
                                                                opponent-position
                                                                opponent-emps
                                                                all-makeable-moves))
                                                       (t (intern (mkstr 'opponent  '- symbol))))))
                      (game       (symbol) (case symbol
                                             (map 'game-map)))
                      (player     (symbol) (values   (case symbol
                                                       (x '(car player-position))
                                                       (y '(cdr player-position))
                                                       (score '(global-score
                                                                player-absolute-x
                                                                game-turn
                                                                player-boosts
                                                                player-lizards
                                                                (cdr player-position)
                                                                player-damage))
                                                       (moves '(remove-impossible-moves
                                                                player-boosts
                                                                player-lizards
                                                                player-trucks
                                                                player-position
                                                                player-emps
                                                                all-makeable-moves))
                                                       (t (intern (mkstr 'player    '- symbol))))))
                      (iteration  (symbol) (values   (intern (mkstr 'iteration '- symbol))))
                      (recur      (iteration-count) `(recur-inner current-game-map
                                                                  player-absolute-x
                                                                  player-position
                                                                  player-boosts
                                                                  player-lizards
                                                                  player-trucks
                                                                  player-emps
                                                                  player-speed
                                                                  player-damage
                                                                  player-boost-counter
                                                                  opponent-absolute-x
                                                                  opponent-position
                                                                  opponent-boosts
                                                                  opponent-lizards
                                                                  opponent-trucks
                                                                  opponent-emps
                                                                  opponent-speed
                                                                  opponent-damage
                                                                  opponent-boost-counter
                                                                  game-turn
                                                                  ,iteration-count)))
             (labels ((recur-inner (current-game-map
                                    player-absolute-x
                                    player-position
                                    player-boosts
                                    player-lizards
                                    player-trucks
                                    player-emps
                                    player-speed
                                    player-damage
                                    player-boost-counter
                                    opponent-absolute-x
                                    opponent-position
                                    opponent-boosts
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
                       (player (absolute-x 1)
                               (position (cons 2 1))
                               (boosts 3)
                               (lizards 4)
                               (trucks 5)
                               (speed 6)
                               (damage 0)
                               (boost-counter 8))
                       (opponent (absolute-x 9)
                                 (position (cons 10 3))
                                 (boosts 11)
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
  `(with-initial-state ,game-state
     (bind (nothing-player-absolute-x
            nothing-player-position
            nothing-player-boosts
            nothing-player-lizards
            nothing-player-trucks
            nothing-player-emps
            nothing-player-speed
            nothing-player-damage
            nothing-player-boost-counter)
       (make-moves
        'nothing
        'nothing
        (setf nothing-player-absolute-x    (player absolute-x)
              nothing-player-position      (player position)
              nothing-player-boosts        (player boosts)
              nothing-player-lizards       (player lizards)
              nothing-player-trucks        (player trucks)
              nothing-player-emps          (player emps)
              nothing-player-speed         (player speed)
              nothing-player-damage        (player damage)
              nothing-player-boost-counter (player boost-counter)))
       (make-moves
        ,move
        'nothing
        (and (equal nothing-player-position (player position))

             (eq nothing-player-absolute-x    (player absolute-x))
             (eq nothing-player-boosts        (player boosts))
             (eq nothing-player-lizards       (player lizards))
             (eq nothing-player-trucks        (player trucks))
             (eq nothing-player-emps          (player emps))
             (eq nothing-player-speed         (player speed))
             (eq nothing-player-damage        (player damage))
             (eq nothing-player-boost-counter (player boost-counter)))))))

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
                         (player boost-counter))
                   found)
             (iter
               (for move in (player moves))
               (when (and (member move all-straight-moves)
                          (truck-infront-of (player position) (game map)))
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
                (player (absolute-x 1)
                        (position (cons 2 1))
                        (boosts 3)
                        (lizards 4)
                        (trucks 5)
                        (speed 6)
                        (damage 0)
                        (boost-counter 8))
                (opponent (absolute-x 9)
                          (position (cons 10 3))
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
        (sort #'> :key (lambda (state) (bind (((path pos-2 _ boosts-2 lizards-2 _ damage-2 _) state))
                                    (global-score (+ (player absolute-x) (car pos-2))
                                                  (+ *current-turn* (length path))
                                                  boosts-2
                                                  lizards-2
                                                  (cdr pos-2)
                                                  damage-2))))))))

(defmacro make-finishing-move (game-state)
  "Optimise for finishing and forget everything else."
  `(bind ((*ahead-of-cache* (make-hash-table :test #'equal)))
     (-> (states-from ,game-state)
       copy-seq
       (sort #'> :key (lambda (state) (car (nth 1 state))))
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
     (bind ((move (cond
                    ((opponent-is-close-by (player absolute-x)
                                           (cdr (player position))
                                           (opponent absolute-x)
                                           (cdr (opponent position)))
                     (bind (((_ _ my-move _ depth) (make-opposed-move ,game-state)))
                       (if (= depth 0) (make-speed-move ,game-state) my-move)))
                    ((close-to-end (player absolute-x)) (make-finishing-move ,game-state))
                    (t (make-speed-move ,game-state))))
            (*ahead-of-cache* (make-hash-table :test #'equal))
            (will-crash       (> (make-moves
                                  'nothing
                                  'nothing
                                  (player damage))
                                 (player damage))))
       (cond
         ((and (no-net-change move ,game-state)
               (< (opponent x) (player x))
               (> (player oils) 0))
          'use_oil)
         ;; TODO: check that we aren't going to crash into a wall! XD
         ((and (> (opponent x) (player x))
               (member (player y) '(1 2))
               (not (eq *previous-move* 'use_emp))
               (> (player emps) 0)
               (not will-crash))
          'use_emp)
         (t move)))))

(defun move-for-state (state)
  "Produce the move which my bot makes from STATE."
  (bind (((player-position . opponent-position) (positions state))

         (game-map                 (rows state))
         (player-absolute-x        (my-abs-x state))
         (opponent-absolute-x      (opponent-abs-x state))
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
         (move                     (determine-move ((game (turn *current-turn*))
                                                    (game-map game-map)
                                                    (player
                                                     (absolute-x player-absolute-x)
                                                     (position player-position)
                                                     (boosts player-boosts)
                                                     (lizards player-lizards)
                                                     (trucks player-trucks)
                                                     (emps player-emps)
                                                     (speed player-speed)
                                                     (damage player-damage)
                                                     (boost-counter player-boost-counter))
                                                    (opponent
                                                     (absolute-x opponent-absolute-x)
                                                     (position opponent-position)
                                                     (boosts opponent-boosts)
                                                     (lizards 1)
                                                     (trucks 1)
                                                     (emps opponent-emps)
                                                     (speed opponent-speed)
                                                     (damage 0)
                                                     (boost-counter 0))))))
    (setf *previous-move* move)
    (format *error-output*
            "My total/average speed: ~a - ~a~%"
            player-absolute-x
            (/ player-absolute-x *current-turn*))
    (format *error-output*
            "Op total/average speed: ~a - ~a~%"
            opponent-absolute-x
            (/ opponent-absolute-x *current-turn*))
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

(defun close-to-end (absolute-x)
  "Produce T if ABSOLUTE-X is close to the edge of the map."
  (> absolute-x 1480))

(defmacro cannot-make-move (boosts lizards trucks pos emps)
  "Produce a function which produces T if MOVE can't be made with BOOSTS, LIZARDS, TRUCKS and EMPS from POS."
  `(lambda (move) (not (move-can-be-made move ,boosts ,lizards ,trucks (cdr ,pos) ,emps))))

(defun remove-impossible-moves (boosts lizards trucks pos emps all-makeable-moves)
  "Remove impossible moves from ALL-MOVES.

Given that the player has BOOSTS, LIZARDS and TRUCKS left and is at
POS."
  (remove-if (cannot-make-move boosts lizards trucks pos emps) all-makeable-moves))

(defun global-score (absolute-x current-turn boosts lizards y damage)
  "Score the position described by ABSOLUTE-X BOOSTS LIZARDS."
  (bind ((is-middle-two (if (or (= y 1)
                                (= y 2))
                            1
                            0)))
    (iter (for coefficients in *heuristic-coeficients*)
      (bind (((x-score
               boosts-score
               lizards-score
               y-score
               damage-score
               current-turn-score) coefficients))
        (maximizing (+ (* x-score             absolute-x)
                       (* boosts-score        boosts)
                       (* lizards-score       lizards)
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
  (bind ((index     (ecase type
                      (mud    0)
                      (boost  1)
                      (wall   2)
                      (tweet  3)
                      (lizard 4)
                      (emp    5)))
         (adj-speed `(case ,move
                       (use_lizard 0)
                       (fix        0)
                       (t          (1- ,speed))))
         (direction `(case ,move
                       (turn_left  'up)
                       (turn_right 'down)
                       (otherwise  'ahead)))
         (adj-x     `(if (eq ,move 'use_lizard)
                         (+ ,speed (car ,pos))
                         (if (eq ,move 'fix)
                             (car ,pos)
                             (ecase ,direction
                               (up    (car ,pos))
                               (down  (car ,pos))
                               (ahead (1+ (car ,pos)))))))
         (adj-y     `(ecase ,direction
                       (up    (if (= 0 (cdr ,pos)) (cdr ,pos) (1- (cdr ,pos))))
                       (down  (if (= 3 (cdr ,pos)) (cdr ,pos) (1+ (cdr ,pos))))
                       (ahead (cdr ,pos)))))
    `(if (eq ,move 'fix) 0
         (aif (gethash (list ,adj-speed ,adj-x ,adj-y) *ahead-of-cache*)
              (aref it ,index)
              (aref (setf (gethash (list ,adj-speed ,adj-x ,adj-y) *ahead-of-cache*)
                          (all-ahead-of ,adj-speed ,game-map ,adj-x ,adj-y))
                    ,index)))))

(defun all-ahead-of (speed game-map x y)
  "Produce a count of all types of powerups and obstacles.

When going at SPEED from X, Y on GAME-MAP."
  (iter
    (for i from (max x 0) below (min (1+ (+ x speed)) (game-map-x-dim game-map)))
    (for tile = (aref-game-map game-map y i))
    (counting (eq 'mud tile)    into muds)
    (counting (eq 'wall tile)   into walls)
    (counting (eq 'boost tile)  into boosts)
    (counting (eq 'lizard tile) into lizards)
    (counting (eq 'tweet tile)  into tweets)
    (counting (eq 'emp tile)    into emps)
    (finally (return (vector muds boosts walls tweets lizards emps)))))

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

(defmacro accumulating-powerups (count-name move type speed game-map position)
  "Produce the value of COUNT-NAME with collected powerups.

Use the state transitions which occur when MOVE is made, finding
powerups of TYPE on the GAME-MAP starting from POSITION."
  `(+ ,count-name
      (if (eq ,move (quote ,(symb 'use_ type))) -1 0)
      (ahead-of ,move ,type ,speed ,game-map ,position)))

;; Known short cuts:
;;  - I don't take boost length into account;
;;
;; TODO: deal with collision state in:
;; "../EntelectChallenge-2020-Overdrive/game-runner/match-logs/2020.08.08.12.33.49"
(defun make-move (move opponent-move game-map
                  opponent-position position speed
                  boosts lizards trucks
                  emps damage boost-counter)
  "Make MOVE across GAME-MAP from POSITION at SPEED with BOOSTS.

Produce the new new position, etc. as values."
  (if (and (eq opponent-move 'use_emp)
           (<= (abs (- (cdr position) (cdr opponent-position))) 1)
           (< (car opponent-position) (car position)))
      (values position 3 boosts lizards trucks emps damage 0)
      (bind ((new-speed         (new-speed move (if (= 1 boost-counter) (min (maximum-speed damage) 9) speed) damage))
             ((x . y)           position)
             (new-x             (new-x x move new-speed damage))
             (new-y             (new-y y move new-speed damage))
             (muds-hit          (ahead-of                      move mud    new-speed game-map position))
             (walls-hit         (ahead-of                      move wall   new-speed game-map position))
             (new-boosts        (accumulating-powerups boosts  move boost  new-speed game-map position))
             (new-lizards       (accumulating-powerups lizards move lizard new-speed game-map position))
             (new-trucks        (accumulating-powerups trucks  move tweet  new-speed game-map position))
             (new-emps          (accumulating-powerups emps    move emp    new-speed game-map position))
             (truck-x           (hit-a-truck game-map x new-x new-y))
             (new-pos           (cons (if truck-x (1- truck-x) new-x) new-y))
             (new-boost-counter (if (or (eq move 'decelerate) (> walls-hit 0) (> muds-hit 0)) 0 (if (eq move 'use_boost) 5 (max 0 (1- boost-counter)))))
             (new-damage        (min 6 (+ muds-hit
                                          (* 2 walls-hit)
                                          (if truck-x 2 0)
                                          (max 0 (if (eq move 'fix) (- damage 2) damage)))))
             (final-speed       (min (maximum-speed new-damage)
                                     (if (or (> walls-hit 0) truck-x)
                                         3
                                         (decrease-speed-by muds-hit new-speed)))))
        (values new-pos final-speed new-boosts new-lizards new-trucks new-emps new-damage new-boost-counter))))

(defun hit-a-truck (game-map start-x end-x new-y)
  "Produce t if you would hit a truck on GAME-MAP from START-X.

NEW-Y is the lane which the car ends up in, and END-X is where you end
up in, in your lane."
  (bind (((_ . trucks)  game-map))
    (iter
      (for (x-truck . y-truck) in trucks)
      (when (and (eq y-truck new-y)
                 (< start-x x-truck)
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

(defun move-can-be-made (move boosts lizards trucks y emps)
  "Produce T if the MOVE can be made from Y coordinate."
  (case move
    (turn_right (< y 3))
    (turn_left  (> y 0))
    (use_boost  (> boosts 0))
    (use_tweet  (> trucks 0))
    (use_lizard (> lizards 0))
    (use_emp    (> emps 0))
    (t          t)))

(defun aref-game-map (game-map y x)
  "Produce the value in GAME-MAP at coordinate Y, X."
  (aref (car game-map) y x))

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

(defmethod rows ((this state))
  "Produce rows as a 2D array of cells from the map in THIS state."
  (iter
    (with world-map = (slot-value this 'world-map))
    (with trucks = '())
    (with result = (make-array (list (length world-map)
                                     (length (aref world-map 0)))
                               :initial-element nil))
    (for y from 0)
    (for row in-vector world-map)
    (iter
      (for cell in-vector row)
      (for x from 0)
      (when (is-occupied-by-cyber-truck cell)
        (push (cons x y) trucks))
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

(defun replay-from-folder (folder-path &rest rounds)
  "Check that `make-move' produces the same result as the target engine."
  (with-consecutive-states folder-path "Quantum" 'A
    (when (or (null rounds)
              (and rounds
                   (member round rounds)))
      (bind ((*ahead-of-cache* (make-hash-table :test #'equal))
             ((:values new-relative-pos
                       new-speed
                       new-boosts
                       new-lizards
                       new-trucks
                       new-emps
                       new-damage
                       new-boost-counter)
              (make-move current-move
                         opponent-move
                         (rows current-state)
                         (cdr (positions current-state))
                         (car (positions current-state))
                         (my-speed current-state)
                         (my-boosts current-state)
                         (my-lizards current-state)
                         (my-trucks current-state)
                         (my-emps   current-state)
                         (my-damage current-state)
                         (my-boost-counter current-state)))
             ((:values opponent-pos)
              (make-move opponent-move
                         current-move
                         (rows current-state)
                         (car (positions current-state))
                         (cdr (positions current-state))
                         (opponent-speed current-state)
                         ;; I don't know how many boosts, lizards or
                         ;; trucks my opponent has(!?)
                         0
                         0
                         0
                         0
                         0
                         2))
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
             (initial    (list (my-abs-pos       current-state)
                               (my-speed         current-state)
                               (my-boosts        current-state)
                               (my-lizards       current-state)
                               (my-trucks        current-state)
                               (my-emps          current-state)
                               (my-damage        current-state)
                               (my-boost-counter current-state)))
             (computed   (list resolved-pos
                               new-speed
                               new-boosts
                               new-lizards
                               new-trucks
                               new-emps
                               new-damage
                               new-boost-counter))
             (actual     (list (my-abs-pos       next-state)
                               (my-speed         next-state)
                               (my-boosts        next-state)
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
                  actual))))))

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
