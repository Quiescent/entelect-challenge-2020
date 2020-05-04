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
                                        (collecting (make-hash-table :test #'eq)))))))
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
                                      (mapcar #'median
                                              (cdr cell))))
                      model))
         (format file "~{~a~^,~},~{~a~^,~}~%"
                 (decode-entry-state-key entry-state-key)
                 distribution))))))

(defun median (xs)
  "Produce the median value in XS."
  (or (iter
        (for (key value) in-hashtable xs)
        (finding key maximizing value))
      -1))

(defun add-to-model (relative-path model)
  "Add all next states from maps in RELATIVE-PATH to MODEL."
  (with-consecutive-states relative-path "random" 'A
    (declare (ignore next-state current-move))
    (iter
      (for (entry-key . distribution) in model)
      (for (speed x y boosts) = (decode-entry-state-key entry-key))
      (iter
        (for i from 0)
        (for move in all-moves)
        (when (move-can-be-made move boosts y)
          (bind ((game-map (rows current-state))
                 (position (cons 4 y))
                 ;; use of x for speed is deliberate.  We're computing
                 ;; how much mud you went through getting into this
                 ;; map
                 (muds-hit (ahead-of mud ahead x game-map position))
                 (entry-speed (decrease-speed-by muds-hit speed))
                 ((:values new-pos new-speed new-boosts)
                  (make-move move
                             (rows current-state)
                             (cons (+ x 4) y)
                             entry-speed
                             boosts)))
            (declare (ignore new-pos new-boosts))
            (incf (gethash new-speed (nth i distribution) 0))))))))

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

(defun fix-entry-key-csv ()
  "Fix the entry-keys in the model csv file."
  (bind ((result
          (with-open-file (file "model.csv")
            (iter
              (with line)
              (while (setq line (mapcar #'read-from-string
                                        (ppcre:split ","
                                                     (read-line file nil)))))
              (for (key . rest) = line)
              (collecting (append (decode-entry-state-key key) rest))))))
    (with-open-file (file "model.csv"
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
      (iter
        (for model in result)
        (format file "~{~a~^,~}~%" model)))))
