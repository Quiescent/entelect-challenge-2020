(in-package :bot)

(defvar *heuristic-coeficients* '((1 1 1 1 1 1 1 1))
  "The coefficients to use when computing the score of a position.")

;; Previous state is list of:
;;  - move,
;;  - map,
;;  - pos,
;;  - boosts,
;;  - lizards,
;;  - trucks,
;;  - speed,
;;  - damage, and
;;  - abs-x.
(defvar *previous-state* '()
  "The state which the player was in last turn.")

(defvar *current-turn* 1
  "The turn of the game that we're on.")

(defvar *banned-move* nil
  "A move which is not allowed, because the state didn't change when it was made.")

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

(defun move-for-state (state)
  "Produce the move which my bot makes from STATE."
  (bind ((map               (rows state))
         ((my-pos . op-pos) (positions state))
         (my-abs-x          (my-abs-x state))
         (opponent-abs-x    (opponent-abs-x state))
         (boosting          (im-boosting state))
         (boosts            (my-boosts state))
         (boost-counter     (my-boost-counter state))
         (lizards           (my-lizards state))
         (trucks            (my-trucks state))
         (speed             (my-speed state))
         (damage            (my-damage state))
         (op-boosts         1)
         (op-speed          (opponent-speed state))
         (current-state     (list map
                                  my-pos
                                  boosts
                                  lizards
                                  trucks
                                  speed
                                  damage
                                  my-abs-x))
         ;; TODO: Check whether we were EMP'd
         (*banned-move*     (when (equal (cdr *previous-state*) current-state) (car *previous-state*)))
         (move              (determine-move map
                                            my-pos
                                            boosting
                                            boosts
                                            lizards
                                            trucks
                                            speed
                                            damage
                                            boost-counter
                                            my-abs-x
                                            opponent-abs-x
                                            op-pos
                                            op-boosts
                                            op-speed)))
    (when *banned-move*
      (format *error-output* "Banning: ~a~%" *banned-move*))
    (format *error-output* "My total/average speed: ~a - ~a~%" my-abs-x            (/ my-abs-x *current-turn*))
    (format *error-output* "Op total/average speed: ~a - ~a~%" opponent-absolute-x (/ opponent-absolute-x *current-turn*))
    (setf *previous-state* (cons move current-state))
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
  "Produce the number of boosts which I have in THIS state."
  (count-if (lambda (x) (string-equal x "LIZARD"))
            (deep-accessor this 'player 'powerups)))

(defmethod my-trucks ((this state))
  "Produce the number of boosts which I have in THIS state."
  (count-if (lambda (x) (string-equal x "TWEET"))
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

(defun determine-move (game-map my-pos boosting boosts
                       lizards trucks speed damage
                       boost-counter my-abs-x opponent-abs-x
                       opponent-pos opponent-boosts opponent-speed)
  "Produce the best move for GAME-MAP.

Given that I'm at MY-POS, whether I'm BOOSTING, how many BOOSTS,
LIZARDS and TRUCKS I have left, the SPEED at which I'm going and
MY-ABS-X position on the board."
  (declare (ignore boosting))
  (cond
    ;; ((and (> trucks 0)
    ;;       (> opponent-speed 3)
    ;;       (opponent-is-close-by my-abs-x (cdr my-pos) opponent-abs-x (cdr opponent-pos)))
    ;;  ;; TODO: don't place the truck on parts of the map that I can't see!!
    ;;  (place-cyber-truck game-map
    ;;                     opponent-pos
    ;;                     1
    ;;                     1
    ;;                     1
    ;;                     opponent-speed
    ;;                     0
    ;;                     my-pos
    ;;                     boosts
    ;;                     lizards
    ;;                     trucks
    ;;                     speed
    ;;                     damage
    ;;                     opponent-abs-x
    ;;                     my-abs-x))
    ;; ((opponent-is-close-by my-abs-x (cdr my-pos) opponent-abs-x (cdr opponent-pos))
    ;;  (make-opposed-move game-map
    ;;                     my-abs-x
    ;;                     my-pos
    ;;                     boosts
    ;;                     lizards
    ;;                     trucks
    ;;                     speed
    ;;                     damage
    ;;                     boost-counter
    ;;                     *my-total-speed*
    ;;                     opponent-abs-x
    ;;                     opponent-pos
    ;;                     opponent-boosts
    ;;                     1
    ;;                     1
    ;;                     opponent-speed
    ;;                     0
    ;;                     *op-total-speed*))
    ((close-to-end my-abs-x)
     (make-finishing-move game-map
                          my-pos
                          boosts
                          lizards
                          trucks
                          speed
                          damage
                          boost-counter))
    (t
     (make-speed-move game-map
                      my-abs-x
                      my-pos
                      boosts
                      lizards
                      trucks
                      speed
                      damage
                      boost-counter))))

(defun close-to-end (absolute-x)
  "Produce T if ABSOLUTE-X is close to the edge of the map."
  (> absolute-x 1480))

(defvar *ahead-of-cache* nil
    "A cache of obstacles ahead of certain points.

Key is (speed x y).

Value is [muds boosts walls tweets lizards].")

(defun place-cyber-truck (game-map
                          op-pos
                          op-boosts
                          op-lizards
                          op-trucks
                          op-speed
                          op-damage
                          my-pos
                          my-boosts
                          my-lizards
                          my-trucks
                          my-speed
                          my-damage
                          opponent-abs-x
                          my-abs-x)
  "Place the truck in front of my opponents best move.

Run minimax from my opponents perspective to find his best move.

The optimiser is run with my the opponent bot at OP-POS, with
OP-BOOSTS, OP-LIZARDS and OP-TRUCKS remaining running at OP-SPEED and
my bot running from MY-POS with MY-BOOSTS at MY-SPEED.

The opponent is at the _absolute_ coordinate:
(OPPONENT-ABS-X, OPPONENT-ABS-Y)."
  ;; Add one to y because their coordinates are 1-based
  nil
  ;; (bind ((op-move (make-opposed-move game-map
  ;;                                    opponent-abs-x
  ;;                                    op-pos
  ;;                                    op-boosts
  ;;                                    op-lizards
  ;;                                    op-trucks
  ;;                                    op-speed
  ;;                                    op-damage
  ;;                                    5
  ;;                                    *op-total-speed*
  ;;                                    my-abs-x
  ;;                                    my-pos
  ;;                                    my-boosts
  ;;                                    my-lizards
  ;;                                    my-trucks
  ;;                                    my-speed
  ;;                                    my-damage
  ;;                                    *my-total-speed*))
  ;;        (*ahead-of-cache* (make-hash-table :test #'equal))
  ;;        ((:values op-pos-2 op-speed-2 op-boosts-2 op-lizards-2 op-trucks-2)
  ;;         (make-move op-move game-map op-pos op-speed op-boosts op-trucks op-speed op-damage 2)))
  ;;   (declare (ignore op-speed-2 op-boosts-2 op-lizards-2 op-trucks-2))
  ;;   ;; Offset by one so that the opponent doesn't land *on* the truck
  ;;   (cons 'use_tweet (cons (+ 1 (- (car op-pos-2) (car op-pos)) opponent-abs-x) (1+ (cdr op-pos-2)))))
  )

(defmacro cannot-make-move (boosts lizards trucks pos)
  "Produce a function which produces T if MOVE can't be made with BOOSTS from POS."
  `(lambda (move) (not (move-can-be-made move ,boosts ,lizards ,trucks (cdr ,pos)))))

(defun remove-impossible-moves (boosts lizards trucks pos all-makeable-moves)
  "Remove impossible moves from ALL-MOVES.

Given that the player has BOOSTS, LIZARDS and TRUCKS left and is at
POS."
  (remove-if (cannot-make-move boosts lizards trucks pos) all-makeable-moves))

(defconstant maximax-depth 2
  "The depth that we should search the game tree.")

(defmacro make-opposed-move (game-state)
  "Produce the best move in GAME-STATE as determined by a few rounds of maximax."
  `(bind ((*ahead-of-cache* (make-hash-table :test #'equal)))
     (with-initial-state ,(cons `(iteration (count ,maximax-depth)) game-state)
       (iter
         (for current-turn = (+ *current-turn* (- maximax-depth (iteration count))))
         (with cells =
               (iter
                 (for player-move in (player moves))
                 (iter
                   (for opponent-move in (opponent moves))
                   (make-moves
                    player-move
                    opponent-move
                    (bind (((player-score
                             opponent-score
                             _
                             _)
                            (if (or (end-state (player position)   game-map)
                                    (end-state (opponent position) game-map)
                                    (= (iteration count) 1))
                                (list (player score)
                                      (opponent score)
                                      nil
                                      nil))))
                      (recur (1- (iteration count)))
                      (finding (list player-score opponent-score player-move opponent-move)
                               minimizing player-score))))))
         (for cell in cells)
         (finding cell maximizing (car cell))))))

;; TODO: remove boost-counter
(defun global-score (absolute-x current-turn boosts lizards y boost-counter damage)
  "Score the position described by ABSOLUTE-X BOOSTS LIZARDS."
  (bind ((is-middle-two (if (or (= y 1)
                                (= y 2))
                            1
                            0)))
    (iter (for coefficients in *heuristic-coeficients*)
      (bind (((x-score
               average-speed-score
               boosts-score
               lizards-score
               y-score
               boost-counter-score
               damage-score
               current-turn-score) coefficients))
        (maximizing (+ (* x-score             absolute-x)
                       (* average-speed-score (/ absolute-x current-turn))
                       (* boosts-score        boosts)
                       (* lizards-score       lizards)
                       (* y-score             is-middle-two)
                       (* boost-counter-score boost-counter)
                       (* damage-score        damage)
                       (* current-turn-score  current-turn)))))))

(defvar all-makeable-moves '(accelerate use_boost turn_right turn_left nothing decelerate use_lizard fix)
  "All the moves which I can make.")

(defvar all-straight-moves '(accelerate use_boost nothing decelerate)
  "All moves which will result in going straight without jumping.")

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
        (player-not-defined 'speed)
        (player-not-defined 'damage)
        (player-not-defined 'boost-counter)

        (opponent-not-defined 'absolute-x)
        (opponent-not-defined 'position)
        (opponent-not-defined 'boosts)
        (opponent-not-defined 'lizards)
        (opponent-not-defined 'trucks)
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
                (player-speed         ,@(cdr (assoc 'speed         player-state)))
                (player-damage        ,@(cdr (assoc 'damage        player-state)))
                (player-boost-counter ,@(cdr (assoc 'boost-counter player-state)))

                (opponent-absolute-x    ,@(cdr (assoc 'absolute-x    opponent-state)))
                (opponent-position      ,@(cdr (assoc 'position      opponent-state)))
                (opponent-boosts        ,@(cdr (assoc 'boosts        opponent-state)))
                (opponent-lizards       ,@(cdr (assoc 'lizards       opponent-state)))
                (opponent-trucks        ,@(cdr (assoc 'trucks        opponent-state)))
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
                                          player-damage-2
                                          player-boost-counter-2)
                                 (make-move ,player-move
                                            current-game-map
                                            player-position
                                            player-speed
                                            player-boosts
                                            player-lizards
                                            player-trucks
                                            player-damage
                                            player-boost-counter))
                                ((:values opponent-position-2-staged
                                          opponent-speed-2
                                          opponent-boosts-2
                                          opponent-lizards-2
                                          opponent-trucks-2
                                          opponent-damage-2
                                          opponent-boost-counter-2)
                                 (make-move ,opponent-move
                                            current-game-map
                                            opponent-position
                                            opponent-speed
                                            opponent-boosts
                                            opponent-lizards
                                            opponent-trucks
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
                                  (player-speed         player-speed-2)
                                  (player-damage        player-damage-2)
                                  (player-boost-counter player-boost-counter-2)

                                  (opponent-absolute-x    opponent-absolute-x-2)
                                  (opponent-position      opponent-position-2)
                                  (opponent-boosts        opponent-boosts-2)
                                  (opponent-lizards       opponent-lizards-2)
                                  (opponent-trucks        opponent-trucks-2)
                                  (opponent-speed         opponent-speed-2)
                                  (opponent-damage        opponent-damage-2)
                                  (opponent-boost-counter opponent-boost-counter-2)

                                  (game-turn (1+ game-turn)))
                             (progn ,@subsequent))))
                      (opponent   (symbol) (values   (case symbol
                                                       (score (global-score opponent-absolute-x
                                                                            game-turn
                                                                            opponent-boosts
                                                                            opponent-lizards
                                                                            (cdr (opponent-position))
                                                                            opponent-boost-counter
                                                                            opponent-damage))
                                                       (moves (remove-impossible-moves opponent-boosts
                                                                                       opponent-lizards
                                                                                       opponent-trucks
                                                                                       opponent-position
                                                                                       all-makeable-moves))
                                                       (intern (mkstr 'opponent  '- symbol)))))
                      (player     (symbol) (values   (case symbol
                                                       (score (global-score player-absolute-x
                                                                            game-turn
                                                                            player-boosts
                                                                            player-lizards
                                                                            (cdr (player-position))
                                                                            player-boost-counter
                                                                            player-damage))
                                                       (moves (remove-impossible-moves player-boosts
                                                                                       player-lizards
                                                                                       player-trucks
                                                                                       player-position
                                                                                       all-makeable-moves))
                                                       (t (intern (mkstr 'player    '- symbol))))))
                      (iteration  (symbol) (values   (intern (mkstr 'iteration '- symbol))))
                      (recur      (iteration-count) `(recur-inner current-game-map
                                                                  player-absolute-x
                                                                  player-position
                                                                  player-boosts
                                                                  player-lizards
                                                                  player-trucks
                                                                  player-speed
                                                                  player-damage
                                                                  player-boost-counter
                                                                  opponent-absolute-x
                                                                  opponent-position
                                                                  opponent-boosts
                                                                  opponent-lizards
                                                                  opponent-trucks
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
                                    player-speed
                                    player-damage
                                    player-boost-counter
                                    opponent-absolute-x
                                    opponent-position
                                    opponent-boosts
                                    opponent-lizards
                                    opponent-trucks
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

(defun minimax-score (my-abs-x op-abs-x)
  "Compute a score me on MY-ABS-X and the opponent is on OP-ABS-X."
  (- my-abs-x op-abs-x))

(defun game-map-y-dim (game-map)
  "Produce the number of squares in the y dimension of GAME-MAP."
  (array-dimension (car game-map) 0))

(defun game-map-x-dim (game-map)
  "Produce the number of squares in the x dimension of GAME-MAP."
  (array-dimension (car game-map) 1))

(defun make-finishing-move (game-map my-pos boosts lizards trucks speed damage boost-counter)
  "Optimise for finishing and forget everything else."
  (bind ((*ahead-of-cache* (make-hash-table :test #'equal)))
    (-> (states-from game-map my-pos speed boosts lizards trucks damage boost-counter)
      copy-seq
      (sort #'> :key (lambda (state) (car (nth 1 state))))
      (stable-sort #'< :key (lambda (state) (length (car state))))
      caar
      last
      car)))

(defun make-speed-move (game-map my-abs-x my-pos boosts lizards trucks speed damage boost-counter)
  "Produce the best speed move to make on GAME-MAP.

Given that I'm at MY-POS, ow many BOOSTS, LIZARDS and TRUCKS I have
left, the SPEED at which I'm going, MY-ABS-X position on the board and
the BOOSTS, LIZARDS and TRUCKS I have left as well as the DAMAGE I've
taken."
  (bind ((*ahead-of-cache* (make-hash-table :test #'equal)))
    (-> (rank-order-all-moves game-map my-abs-x my-pos boosts lizards trucks speed damage boost-counter)
      caar
      last
      car)))

;; TODO: remove arbitrary constraints like boosting etc.
(defun rank-order-all-moves (game-map my-abs-x my-pos boosts lizards trucks speed damage boost-counter)
  "Produce all the moves from GAME-MAP ordered by best placement on the global map.

Given that I'm at MY-POS, ow many BOOSTS, LIZARDS and TRUCKS I have
left, the SPEED at which I'm going and MY-ABS-X position on the
board."
  (bind ((*ahead-of-cache* (make-hash-table :test #'equal)))
    (-> (states-from game-map my-pos speed boosts lizards trucks damage boost-counter)
      (remove-fixing-at-full-health damage)
      (trim-to-two-moves game-map my-pos boosts lizards trucks speed damage boost-counter)
      (boosting-results-in-two-rounds-at-15 game-map my-pos boosts lizards trucks speed damage boost-counter)
      (removing-no-net-change game-map my-pos boosts lizards trucks speed damage boost-counter)
      copy-seq
      (stable-sort #'> :key (lambda (state) (if (eq (-> state car last car) 'use_boost) 0 1)))
      (stable-sort #'> :key (lambda (state) (car (nth 1 state))))
      (stable-sort #'> :key (lambda (state) (nth 2 state)))
      (stable-sort #'> :key (lambda (state) (bind (((path pos-2 _ boosts-2 lizards-2 _ damage-2 boost-counter-2) state))
                                         (global-score (+ my-abs-x (car pos-2))
                                                       (+ *current-turn* (length path))
                                                       boosts-2
                                                       lizards-2
                                                       (cdr pos-2)
                                                       boost-counter-2
                                                       damage-2)))))))

(defun removing-no-net-change (end-states game-map pos boosts lizards trucks speed damage boost-counter)
  "Remove END-STATES which didn't have a net change after the first move was made."
  (remove-if (lambda (end-state)
               (bind ((first-move (-> (car end-state) last car))
                      ((:values pos-1 speed-1 boosts-1 lizards-1 trucks-1 damage-1)
                       (make-move first-move game-map pos speed boosts lizards trucks damage boost-counter)))
                 (declare (ignore boosts-1 lizards-1))
                 (and (equal pos     pos-1)
                      (eq    speed   speed-1)
                      (eq    trucks  trucks-1)
                      (eq    damage  damage-1))))
             end-states))

(defun remove-fixing-at-full-health (end-states damage)
  "Remove paths beginning 'FIX in END-STATES if I have zero DAMAGE."
  (if (= damage 0)
      (remove-if (lambda (end-state) (eq 'fix (-> (car end-state) last car))) end-states)
      end-states))

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

(defun trim-to-two-moves (end-states game-map pos boosts lizards trucks speed damage boost-counter)
  "Trim all end states to two moves deep.

Use GAME-MAP POS, BOOSTS LIZARDS TRUCKS and SPEED to make moves from
the starting state."
  (iter
    (for (path . rest) in end-states)
    (when (< (length path) 2)
      (collecting (cons path rest))
      (next-iteration))
    (for move-1 = (nth (- (length path) 1) path))
    (for move-2 = (nth (- (length path) 2) path))
    (bind (((:values pos-1 speed-1 boosts-1 lizards-1 trucks-1 damage-1 boost-counter-1)
            (make-move move-1 game-map pos speed boosts lizards trucks damage boost-counter))
           ((:values pos-2 speed-2 boosts-2 lizards-2 trucks-2 damage-2 boost-counter-2)
            (make-move move-2 game-map pos-1 speed-1 boosts-1 lizards-1 trucks-1 damage-1 boost-counter-1)))
      (collecting (list (list move-2 move-1)
                        pos-2
                        speed-2
                        boosts-2
                        lizards-2
                        trucks-2
                        damage-2
                        boost-counter-2)))))

(defun boosting-results-in-two-rounds-at-15 (end-states game-map pos boosts lizards trucks speed damage boost-counter)
  "Only consider boost moves when we don't boost through mud.

Use GAME-MAP POS, BOOSTS LIZARDS TRUCKS, SPEED and DAMAGE to make moves from
the starting state."
  (iter
    (for (path . rest) in end-states)
    (when (< (length path) 2)
      (collecting (cons path rest))
      (next-iteration))
    (for move-1 = (nth (- (length path) 1) path))
    (for move-2 = (nth (- (length path) 2) path))
    (bind (((:values pos-1 speed-1 boosts-1 lizards-1 trucks-1 damage-1 boost-counter-1)
            (make-move move-1 game-map pos speed boosts lizards trucks damage boost-counter))
           ((:values pos-2 speed-2 boosts-2 lizards-2 trucks-2 damage-2 boost-counter-2)
            (make-move move-2 game-map pos-1 speed-1 boosts-1 lizards-1 trucks-1 damage-1 boost-counter-1)))
      (when (or (not (eq move-1 'USE_BOOST))
                (or (eq speed 3)
                    (eq boosts-1 boosts)
                    (and (eq speed-1 15)
                         (eq speed-2 15))))
        ;; Bookmark: I need to start analysing from round: 51 in the .59 match
        ;; Maybe when move-2 is boost I should filter it out if it boosts through mud
        (collecting (list (list move-2 move-1)
                          pos-2
                          speed-2
                          boosts-2
                          lizards-2
                          trucks-2
                          damage-2
                          boost-counter-2))))))

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
(defun states-from (game-map my-pos speed boosts lizards trucks damage boost-counter)
  "Produce all possible states using GAME-MAP.

Where my car is at MY-POS and is going at SPEED and I have BOOSTS,
LIZARDS and TRUCKS left.

First element of a path is the path taken.
Second is my current position.
Third is my speed.
Fourth is my boosts left.
Fifth is my lizards left.
Sixth is my trucks left.
Seventh is my damage.
Eighth is my boost counter."
  (iter
    (with counter = 0)
    (with paths-to-explore = (list (list nil my-pos speed boosts lizards trucks damage boost-counter)))
    (with explored = (make-hash-table :test #'equal))
    (with found-paths)
    (while (and (not (null paths-to-explore))
                (< counter 100000)))
    (bind (((path current-pos current-speed current-boosts current-lizards current-trucks current-damage current-boost-counter)
            (pop paths-to-explore)))
      (when (> (length path) 3)
        (push (list path current-pos current-speed current-boosts current-lizards current-trucks current-damage current-boost-counter)
              found-paths)
        (next-iteration))
      (when (gethash path explored)
        (next-iteration))
      (if (end-state current-pos game-map)
          (progn
            (push (list path current-pos current-speed current-boosts current-lizards current-trucks current-damage current-boost-counter)
                  found-paths))
          (iter
            (for move in (remove-impossible-moves current-boosts
                                                  current-lizards
                                                  current-trucks
                                                  current-pos
                                                  all-makeable-moves))
            (when (or (and (null path)
                           (eq move *banned-move*))
                      (and (member move all-straight-moves)
                           (truck-infront-of current-pos game-map)))
              (next-iteration))
            (bind (((:values new-pos new-speed new-boosts new-lizards new-trucks new-damage new-boost-counter)
                    (make-move move game-map current-pos current-speed current-boosts current-lizards current-trucks current-damage current-boost-counter)))
              (push (list (cons move path) new-pos new-speed new-boosts new-lizards new-trucks new-damage new-boost-counter)
                    paths-to-explore)))))
    (incf counter)
    (finally (return found-paths))))

(defun truck-infront-of (current-pos game-map)
  "Produce t if there is a truck immediately in front of CURRENT-POS on GAME-MAP."
  (bind (((_ . trucks) game-map)
         ((x . y)      current-pos))
    (iter
      (for (x-truck . y-truck) in trucks)
      (thereis (and (eq y-truck y)
                    (eq x (1- x-truck)))))))

;; Paul Graham: On Lisp
(eval-when (:compile-toplevel
            :load-toplevel
            :execute)

  (defun mkstr (&rest args)
    (with-output-to-string (s)
      (dolist (a args) (princ a s))))

  (defun symb (&rest args)
    (values (intern (apply #'mkstr args)))))

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
                      (lizard 4)))
         (adj-speed `(if (or (eq ,move 'use_lizard)
                             (eq ,move 'fix))
                         0
                         (1- ,speed)))
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
    `(aif (gethash (list ,adj-speed ,adj-x ,adj-y) *ahead-of-cache*)
          (aref it ,index)
          (aref (setf (gethash (list ,adj-speed ,adj-x ,adj-y) *ahead-of-cache*)
                      (all-ahead-of ,adj-speed ,game-map ,adj-x ,adj-y))
                ,index))))

(defun all-ahead-of (speed game-map x y)
  "Produce a count of all types of powerups and obstacles.

When going at SPEED from X, Y on GAME-MAP."
  (if (= speed 0)
      (vector 0 0 0 0 0)
      (iter
        (for i from (max x 0) below (min (1+ (+ x speed)) (game-map-x-dim game-map)))
        (for tile = (aref-game-map game-map y i))
        (counting (eq 'mud tile)    into muds)
        (counting (eq 'wall tile)   into walls)
        (counting (eq 'boost tile)  into boosts)
        (counting (eq 'lizard tile) into lizards)
        (counting (eq 'tweet tile)  into tweets)
        (finally (return (vector muds boosts walls tweets lizards))))))

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
(defun make-move (move game-map position speed boosts lizards trucks damage boost-counter)
  "Make MOVE across GAME-MAP from POSITION at SPEED with BOOSTS.

Produce the new new position, etc. as values."
  (bind ((new-speed         (new-speed move (if (= 1 boost-counter) (min (maximum-speed damage) 9) speed) damage))
         ((x . y)           position)
         (new-x             (new-x x move new-speed damage))
         (new-y             (new-y y move new-speed damage))
         (muds-hit          (ahead-of                      move mud    new-speed game-map position))
         (walls-hit         (ahead-of                      move wall   new-speed game-map position))
         (new-boosts        (accumulating-powerups boosts  move boost  new-speed game-map position))
         (new-lizards       (accumulating-powerups lizards move lizard new-speed game-map position))
         (new-trucks        (accumulating-powerups trucks  move tweet  new-speed game-map position))
         (truck-x           (hit-a-truck game-map x new-x new-y))
         (new-pos           (cons (if truck-x (1- truck-x) new-x) new-y))
         (new-boost-counter (max 0 (1- boost-counter)))
         (new-damage        (min 6 (+ muds-hit
                                      (* 2 walls-hit)
                                      (if truck-x 2 0)
                                      (max 0 (if (eq move 'fix) (- damage 2) damage)))))
         (final-speed       (min (maximum-speed new-damage)
                                 (if (or (> walls-hit 0) truck-x)
                                     3
                                     (decrease-speed-by muds-hit new-speed)))))
    (values new-pos final-speed new-boosts new-lizards new-trucks new-damage new-boost-counter)))

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
              (8 'tweet))))
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

(defun compare-rankings-to-opponent-move (folder-path opponent-name opponent-player)
  "Compare my decisions to the opponents for each round in FOLDER-PATH."
  (with-consecutive-states folder-path opponent-name opponent-player
    (declare (ignore opponent-move next-state))
    (format t "========================================~%")
    (format t "==============Round: ~a=============~%~%" round)
    (bind ((*ahead-of-cache*   (make-hash-table :test #'equal))
           (game-map           (rows current-state))
           ((my-pos . op-pos)  (positions current-state))
           (my-abs-x           (my-abs-x current-state))
           (opponent-abs-x     (opponent-abs-x current-state))
           (boosting           (im-boosting current-state))
           (boosts             (my-boosts current-state))
           (lizards            (my-lizards current-state))
           (trucks             (my-trucks current-state))
           (speed              (my-speed current-state))
           (damage             (my-damage current-state))
           (boost-counter      (my-boost-counter current-state))
           (op-boosts          1)
           (op-speed           (opponent-speed current-state))
           (move-i-would-make  (determine-move game-map
                                               my-pos
                                               boosting
                                               boosts
                                               lizards
                                               trucks
                                               speed
                                               damage
                                               boost-counter
                                               my-abs-x
                                               opponent-abs-x
                                               op-pos
                                               op-boosts
                                               op-speed))
           (ranked-speed-moves (rank-order-all-moves game-map
                                                     my-abs-x
                                                     my-pos
                                                     boosts
                                                     lizards
                                                     trucks
                                                     speed
                                                     damage
                                                     boost-counter)))
      (format t "I'm making a ~a move.~%"
              (cond
                ((and (> trucks 0)
                      (> op-speed 3)
                      (opponent-is-close-by my-abs-x (cdr my-pos) opponent-abs-x (cdr op-pos)))
                 'cyber)
                ((opponent-is-close-by my-abs-x (cdr my-pos) opponent-abs-x (cdr op-pos))
                 'opposed)
                (t 'speed)))
      (format t "I would make:  ~a~%"   move-i-would-make)
      (format t "Op made:       ~a~%~%" current-move)
      (format t
              "How I rank speed moves:~% ~{~a~^~%~}~%"
              ranked-speed-moves)
      (format t "========================================~%"))))

(defun replay-from-folder (folder-path)
  "Check that `make-move' produces the same result as the target engine."
  (with-consecutive-states folder-path "Quantum" 'A
    (bind ((*ahead-of-cache* (make-hash-table :test #'equal))
           ((:values new-relative-pos new-speed new-boosts new-lizards new-trucks new-damage)
            (make-move current-move
                       (rows current-state)
                       (car (positions current-state))
                       (my-speed current-state)
                       (my-boosts current-state)
                       (my-lizards current-state)
                       (my-trucks current-state)
                       (my-damage current-state)
                       (my-boost-counter current-state)))
           ((:values opponent-pos)
            (make-move opponent-move
                       (rows current-state)
                       (cdr (positions current-state))
                       (opponent-speed current-state)
                       ;; I don't know how many boosts, lizards or
                       ;; trucks my opponent has(!?)
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
           (initial    (list (my-abs-pos current-state)
                             (my-speed current-state)
                             (my-boosts current-state)
                             (my-lizards current-state)
                             (my-trucks current-state)
                             (my-damage current-state)))
           (computed   (list resolved-pos new-speed new-boosts new-lizards new-trucks new-damage))
           (actual     (list (my-abs-pos next-state)
                             (my-speed next-state)
                             (my-boosts next-state)
                             (my-lizards next-state)
                             (my-trucks next-state)
                             (my-damage next-state))))
      (when (not (equal computed actual))
        (format t "~s:~6T~a ~25T ~a ~40T~a /~65T~a~%"
                round
                initial
                current-move
                computed
                actual)))))

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
