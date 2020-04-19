(in-package :bot)

(defun find-data (folder-path objective-round)
  "Produce features for all games in FOLDER-PATH.

Produce T for the label if the game was won in less than the
OBJECTIVE-ROUND."
  (with-open-file (file "/.../filename.txt"
                        :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create)
    (iter
      (for match-path in (matches-where-i-won folder-path))
      (iter
        (for result in (gather-statistics (concatenate 'string
                                                       "../"
                                                       (subseq match-path 17))
                                          objective-round))
        (format file "~{~a~^, ~}~%" result)))))

(defun gather-statistics (folder-path objective-round)
  "Produce features for states in FOLDER-PATH.

The features are a collection of metadata extracted from each state.
Whether they resulted in a finishing round less than OBJECTIVE-ROUND
is the binominal label."
  (bind ((less-than-objective (finished-in-less-than objective-round folder-path))
         results)
    (with-consecutive-states folder-path "Quantum" 'A
      (declare (ignore next-state))
      (bind ((my-abs-pos  (my-abs-pos current-state))
             (my-speed    (my-speed current-state))
             (my-boosts   (my-boosts current-state))
             (my-pos      (car (positions current-state)))
             ((_ . y)     my-pos)
             (new-speed   (case current-move
                            (accelerate (increase-speed my-speed))
                            (use_boost  15)
                            (otherwise  my-speed)))
             (game-map    (rows current-state))
             (mud-ahead   (ahead-of mud ahead new-speed game-map my-pos))
             (mud-up      (if (> y 0) (ahead-of mud up   new-speed game-map my-pos) most-positive-fixnum))
             (mud-down    (if (< y 3) (ahead-of mud down new-speed game-map my-pos) most-positive-fixnum))
             (speed-ahead (ahead-of speed ahead new-speed game-map my-pos))
             (speed-up    (if (> y 0) (ahead-of speed up   new-speed game-map my-pos) most-positive-fixnum))
             (speed-down  (if (< y 3) (ahead-of speed down new-speed game-map my-pos) most-positive-fixnum)))
        (push (list my-abs-pos
                    my-speed
                    my-boosts
                    mud-ahead
                    mud-up
                    mud-down
                    speed-ahead
                    speed-up
                    speed-down
                    current-move
                    less-than-objective)
              results)))
    results))

(defun finished-in-less-than (objective-round relative-folder-path)
  "Produce T if the game in RELATIVE-FOLDER-PATH finished faster than OBJECTIVE-ROUND."
  (< (- (length (directory (make-pathname :directory
                                          (list :relative relative-folder-path)
                                          :name :wild
                                          :type :wild))) 2)
     objective-round))

(defun matches-where-i-won (relative-folder-path)
  "Produce all match folders in RELATIVE-FOLDER-PATH where `Quantum' won."
  (remove-if-not #'i-won-match
                 (mapcar #'namestring
                         (directory (make-pathname :directory
                                                   (list :relative relative-folder-path)
                                                   :name :wild
                                                   :type :wild)))))

(defun i-won-match (folder-path)
  "Produce T if I won the match in FOLDER-PATH."
  (bind ((final-round    (last-round-folder folder-path))
         (end-state-path (concatenate 'string final-round "endGameState.txt")))
    (and final-round
         (probe-file end-state-path)
         (with-open-file (file end-state-path)
           (iter
             (with line)
             (setf line (read-line file nil))
             (while line)
             (finding t such-that (equal line "The winner is: A - Quantum")))))))

(defun last-round-folder (absolute-folder-path)
  "Produce the folder for the last round for the log inside of ABSOLUTE-FOLDER-PATH."
  (car (last (sort (remove-if-not #'is-round-folder-path
                                  (mapcar #'namestring
                                          (directory (make-pathname :directory
                                                                    (list :absolute absolute-folder-path)
                                                                    :name :wild
                                                                    :type :wild))))
                   #'string-lessp))))

(defun is-round-folder-path (folder-path)
  "Produec T if FOLDER-PATH is a round folder from a run of the game."
  (cl-ppcre:scan "Round [0-9][0-9][0-9]/$" folder-path))
