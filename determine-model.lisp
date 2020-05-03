(in-package :bot)

(defun create-model (folder-path)
  "Produce a model for the score of every possible exit from a map.

There are a number of terms mentioned in that documentation sentence
which require some explanation.  Let's try to explain it by describing
the algorithm.

For each possible state that a map can be transitioned into and for
every map in a collection of randomly generated maps, what is the
distribution of likely states that the bot could end up in."
  (iter
    (with model = (iter
                    (for (speed x y boosts) in (all-possible-entry-states))
                    (collecting (cons (encode-entry-state-key speed x y boosts)
                                      (iter (for _ in all-moves)
                                        (collecting (cons 0 0)))))))
    (for match-path in (matches-where-i-won folder-path))
    (for i from 0)
    (format t "Working on game: ~a~%" i)
    (add-to-model (concatenate 'string "../" (subseq match-path 17)) model)
    (finally
     (with-open-file (file "model.csv"
                           :direction :output
                           :if-exists :supersede
                           :if-does-not-exist :create)
       (iter
         (for (entry-state-key . distribution) in
              (mapcar (lambda (cell) (cons (car cell)
                                           (/ (cadr cell) (cddr cell))))
                      model))
         (format file "~a,~{~a~^,~}~%" entry-state-key distribution))))))

(defun add-to-model (relative-path model)
  "Add all next states from maps in RELATIVE-PATH to MODEL."
  (with-consecutive-states relative-path "random" 'A
    (declare (ignore next-state current-move))
    (iter
      (for (entry-key . distribution) in model)
      (for (speed x y boosts) = (decode-entry-state-key entry-key))
      (iter
        (for cell in distribution)
        (for move in all-moves)
        (when (move-can-be-made move boosts y)
         (bind (((:values new-pos new-speed new-boosts)
                 (make-move move
                            (rows current-state)
                            (cons (+ x 4) y)
                            (my-speed current-state)
                            (my-boosts current-state))))
           (declare (ignore new-pos new-boosts))
           (incf (car cell) new-speed)
           (incf (cdr cell))))))))

(defun encode-entry-state-key (speed x y boosts)
  "Encode SPEED, X, Y and BOOSTS as a key for an entry state."
  (logior speed
          (ash x 5)
          (ash y 10)
          (if (> boosts 0) (ash 1 15) 0)))

(defun decode-entry-state-key (key)
  "Decode KEY into a (SPEED X Y)."
  (list (logand 15 key)
        (ash (logand key 992)        -5)
        (ash (logand key 31744)      -10)
        (ash (logand key (ash 1 15)) -15)))

(defun all-possible-entry-states ()
  "Produce all the possible states that a bot could transition into a state in."
  (iter outer
    (for speed in '(3 6 8 9 15))
    (iter
      (for y in '(0 1 2 3))
      (iter
        (for x in '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14))
        (iter
          (for boosts in '(0 1))
          (in outer (collecting (list speed x y boosts))))))))
