(in-package :bot)

(defun find-data (folder-path objective-round)
  "Produce features for all games in FOLDER-PATH.

Produce T for the label if the game was won in less than the
OBJECTIVE-ROUND."
  (with-open-file (file "data.csv"
                        :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create)
    (iter
      (for i from 0)
      (for match-path in (matches-where-i-won folder-path))
      (format t "Working on game: ~a~%" i)
      (iter
        (for result in (gather-statistics (concatenate 'string
                                                       "../"
                                                       (subseq match-path 17))
                                          objective-round))
        (format file "~{~a~^,~}~%" result)))))

(defconstant blocks-to-end-of-map 20
  "The count of blocks in front of the car.")

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
             (game-map    (rows current-state))
             (mud-ahead   (ahead-of mud ahead blocks-to-end-of-map game-map my-pos))
             (mud-up      (if (> y 0)
                              (ahead-of mud up   blocks-to-end-of-map game-map my-pos)
                              most-positive-fixnum))
             (mud-down    (if (< y 3)
                              (ahead-of mud down blocks-to-end-of-map game-map my-pos)
                              most-positive-fixnum))
             (speed-ahead (ahead-of speed ahead blocks-to-end-of-map game-map my-pos))
             (speed-up    (if (> y 0)
                              (ahead-of speed up   blocks-to-end-of-map game-map my-pos)
                              most-positive-fixnum))
             (speed-down  (if (< y 3)
                              (ahead-of speed down blocks-to-end-of-map game-map my-pos)
                              most-positive-fixnum)))
        (push (list (car my-abs-pos)
                    (cdr my-abs-pos)
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
    (reverse results)))

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
  (car (last (sort (round-folders absolute-folder-path) #'string-lessp))))

(defun round-folders (absolute-folder-path)
  "Produce the folders for rounds in ABSOLUTE-FOLDER-PATH."
  (remove-if-not #'is-round-folder-path
                 (mapcar #'namestring
                         (directory (make-pathname :directory
                                                   (list :absolute absolute-folder-path)
                                                   :name :wild
                                                   :type :wild)))))

(defun is-round-folder-path (folder-path)
  "Produec T if FOLDER-PATH is a round folder from a run of the game."
  (cl-ppcre:scan "Round [0-9][0-9][0-9]/$" folder-path))

(defun average-finishing-round (relative-folder-path)
  "Produce the average game length of matches in RELATIVE-FOLDER-PATH."
  (iter
    (for round-folder in (directory (make-pathname :directory
                                                   (list :relative relative-folder-path)
                                                   :name :wild
                                                   :type :wild)))
    (for round-count = (count-rounds (namestring round-folder)))
    (summing round-count into total-rounds)
    (counting round-count into total-games)
    (finally (return (float (/ total-rounds total-games))))))

(defun count-rounds (absolute-folder-path)
  "Produce a count of the rounds in ABSOLUTE-FOLDER-PATH."
  (length (round-folders absolute-folder-path)))
