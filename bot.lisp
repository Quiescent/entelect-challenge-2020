(in-package :bot)

(defun main () 
  (loop while t
     for round-number = (read-line)
     for state = (load-state-file round-number)
     for map = (rows state)
     for (my-pos . _) = (positions state)
     for boosting = (im-boosting state)
     for boosts = (my-boosts state)
     for move = (determine-move map my-pos boosting boosts)
     do (format t "C;~a;~a" (current-round state) move)))

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
  (deep-accessor this 'player 'boost-counter))

(defun determine-move (game-map my-pos boosting boosts)
  "Produce the best move for GAME-MAP.

Given that I'm at MY-POS, whether I'm BOOSTING and how many BOOSTS I
have left."
  (bind (((x y)      my-pos)
         (speed-up   (if (> y 0) (speed-ahead-of game-map x (1- y)) 0))
         (speed-down (if (< y 3) (speed-ahead-of game-map x (1+ y)) 0))
         (no-boosts  (< boosts 1)))
    (cond
      ((and (not boosting) boosts) 'boost)
      ((and no-boosts speed-up)    'turn-left)
      ((and no-boosts speed-down)  'turn-right)
      (t                           'accelerate))))

(defconstant row-length 26
  "The number of squares visible in a row.")

(defun mud-ahead-of (game-map x y)
  "Produce the count of mud on GAME-MAP ahead of (X, Y)."
  (iter
    (for i from x below row-length)
    (counting (eq 'mud (aref game-map y i)))))

(defun speed-ahead-of (game-map x y)
  "Produce the count of boosts on GAME-MAP ahead of (X, Y)."
  (iter
    (for i from x below row-length)
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
  (cons (position-to-cons (deep-accessor this 'player   'map-position))
        (position-to-cons (deep-accessor this 'opponent 'map-position))))
