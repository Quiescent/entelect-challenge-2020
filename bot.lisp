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
         (boosting      (im-boosting state))
         (boosts        (my-boosts state))
         (speed         (my-speed state)))
    (determine-move map my-pos boosting boosts speed)))

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

(defmacro logging-cond (&rest forms)
  "Print the condition which ended up being true in cond."
  `(cond
     ,@(mapcar (lambda (form) `(,(car form) (progn (print (quote ,(car form)) *error-output*)
                                              ,(cadr form))))
               forms)))

(defun determine-move (game-map my-pos boosting boosts speed)
  "Produce the best move for GAME-MAP.

Given that I'm at MY-POS, whether I'm BOOSTING, how many BOOSTS I have
left and the SPEED at which I'm going."
  (bind (((x . y)           my-pos)
         (mud-here          (mud-ahead-of game-map x y))
         (mud-up            (if (> y 0) (mud-ahead-of game-map x (1- y)) 0))
         (mud-down          (if (< y 3) (mud-ahead-of game-map x (1+ y)) 0))
         (too-much-mud-here (>= 3 mud-here))
         (too-much-mud-up   (>= 3 mud-up))
         (too-much-mud-down (>= 3 mud-down))
         (speed-up          (if (> y 0) (speed-ahead-of game-map x (1- y)) 0))
         (speed-down        (if (< y 3) (speed-ahead-of game-map x (1+ y)) 0))
         (speed-here        (speed-ahead-of game-map x y))
         (gap-here          (gap-ahead-of speed game-map x y))
         (gap-up            (and (> y 0) (gap-ahead-of speed game-map x (1- y))))
         (gap-down          (and (< y 3) (gap-ahead-of speed game-map x (1+ y))))
         (no-boosts         (< boosts 1)))
    (logging-cond
     ((and (not boosting)
           (not too-much-mud-here)
           (> boosts 0)
           (or gap-here (not (or gap-up gap-down))))
      'use_boost)
     ((and no-boosts
           (not too-much-mud-here)
           (> speed-here 0))
      'accelerate)
     ((and no-boosts
           (not too-much-mud-up)
           (> speed-up 0))
      'turn_left)
     ((and no-boosts
           (not too-much-mud-down)
           (> speed-down 0))
      'turn_right)
     ((and gap-up
           (not too-much-mud-up)
           (not gap-here))
      'turn_left)
     ((and gap-down
           (not too-much-mud-down)
           (not gap-here))
      'turn_right)
     (t 'accelerate))))

(defun gap-ahead-of (speed game-map x y)
  "Produce T if there are SPEED clear blocks in GAME-MAP ahead of (X, Y)."
  (iter
    (for i from x below (min (+ x speed) (array-dimension game-map 1)))
    (counting (not (eq 'mud (aref game-map y i))))))

(defconstant row-length 26
  "The number of squares visible in a row.")

(defun mud-ahead-of (game-map x y)
  "Produce the count of mud on GAME-MAP ahead of (X, Y)."
  (iter
    (for i from x below (array-dimension game-map 1))
    (counting (eq 'mud (aref game-map y i)))))

(defun speed-ahead-of (game-map x y)
  "Produce the count of boosts on GAME-MAP ahead of (X, Y)."
  (iter
    (for i from x below (array-dimension game-map 1))
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
