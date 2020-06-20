(in-package :bot)

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
