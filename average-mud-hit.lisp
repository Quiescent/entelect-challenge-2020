(in-package :bot)

(defun average-hits-in-games (relative-folder-path player which-player)
  "For all games in PATH, produce the average cumulative mud and walls hit by PLAYER.

Player has the tag WHICH-PLAYER."
  (iter
    (for match-path in (all-matches relative-folder-path))
    (format t "Working on: ~a~%" match-path)
    (for relative-match-path = (concatenate 'string "../" (subseq match-path (+ 4 (search "wip/" match-path)))))
    (collecting (->> (mud-and-walls-hit-per-round relative-match-path player which-player)
                  (reverse)
                  (accumulate-muds-and-walls)
                  (reverse))
                :into muds-and-walls-lists)
    (finally (return (average-muds-and-walls muds-and-walls-lists)))))

(defun all-matches (relative-folder-path)
  "Produce all match folders in RELATIVE-FOLDER-PATH."
  (mapcar #'namestring
          (directory (make-pathname :directory
                                    (list :relative relative-folder-path)
                                    :name :wild
                                    :type :wild))))

(defconstant map-length 1500
  "The x-length of the map.")

(defun mud-and-walls-hit-per-round (path player which-player)
  "Scan through PATH, finding games played by PLAYER with label WHICH-PLAYER."
  (bind ((counts (make-array map-length :initial-element (cons 0 0))))
    (with-consecutive-states path player which-player
             (declare (ignore next-state opponent-move opponent-speed opponent-boosts))
             (bind ((*ahead-of-cache* (make-hash-table :test #'equal))
                    (game-map         (rows current-state))
                    (position         (car (positions current-state)))
                    (abs-x            (car (my-abs-pos current-state)))
                    (speed            (my-speed current-state))
                    (new-speed        (new-speed current-move speed))
                    (muds-hit         (ahead-of current-move mud  new-speed game-map position))
                    (walls-hit        (ahead-of current-move wall new-speed game-map position)))
               (when (< abs-x map-length)
                (setf (aref counts abs-x) (cons muds-hit walls-hit)))))
    counts))

(defun accumulate-muds-and-walls (muds-and-walls)
  "Produce a cumulative sum of the (mud . wall) cons cells MUDS-AND-WALLS."
  (iter
    (with mud-sum  = 0)
    (with wall-sum = 0)
    (for (mud . wall) in-vector muds-and-walls)
    (incf mud-sum  mud)
    (incf wall-sum wall)
    (collecting (cons mud-sum wall-sum))))

(defun average-muds-and-walls (muds-and-walls-lists)
  "Average the values per-position in MUDS-AND-WALLS-LISTS."
  (iter
    (with counts = (iter
                     (for i from 0 to map-length)
                     (collect (cons 0 0) :result-type 'vector)))
    (with games  = (length muds-and-walls-lists))
    (for muds-and-walls in muds-and-walls-lists)
    (iter
      (for idx from 0)
      (for (mud . wall) in muds-and-walls)
      (for cell = (aref counts idx))
      (incf (car cell) (/ mud  games))
      (incf (cdr cell) (/ wall games)))
    (finally (return counts))))
