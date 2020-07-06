(ql:quickload :metabang-bind)
(ql:quickload :iterate)

(defpackage optimise-bot
  (:use :cl :iterate :metabang-bind))

(in-package :optimise-bot)

(defvar population-size 1000
  "The number of solutions in the population.")

(defvar *directory-of-bot-to-optimise* "home/edward/wip/entelect-challenge-2020/bots/to-optimise"
  "The directory of the bot being optimised.")

(defvar *game-runner-dir* "/home/edward/wip/EntelectChallenge-2020-Overdrive/game-runner"
  "The directory where the game runner lives.")

;; NOTE: You must setup the bots to run as do-nothing and to-optimise!!!
(defun optimise-score ()
  "Use differential evolution to find good score weights for the bot."
  (bind ((individuals (seed-population))
         (scores      (mapcar #'fitness individuals))
         (zipped      (mapcar #'cons scores individuals))
         (sorted      (sort zipped #'> :key #'car)))
    (with-open-file (f (make-pathname :directory
                                      (list :absolute
                                            *directory-of-bot-to-optimise*)
                                      :name "results")
                       :if-exists :supersede
                       :if-does-not-exist :create
                       :direction :output)
      (format f "'~A" sorted))
    (car sorted)))

(defun seed-population ()
  "Create a population of vectors for coefficients for the global scare heuristic."
  (iter
    (for i from 0 below population-size)
    (collecting (list (/ (random 1000) 1000)
                      (/ (random 1000) 1000)
                      (/ (random 1000) 1000)
                      (/ (random 1000) 1000)))))

(defun fitness (optimisation-vector)
  "Produce a measure of the fitness of OPTIMISATION-VECTOR."
  (progn
    (write-config optimisation-vector)
    (uiop:chdir *game-runner-dir*)
    (iter
      (for i from 0 below 5)
      (format t "[~a/5]: ~a~%" (1+ i) optimisation-vector)
      (uiop:delete-directory-tree (make-pathname :directory
                                                 (list :absolute
                                                       *game-runner-dir*
                                                       "match-logs"))
                                  :validate t
                                  :if-does-not-exist :ignore)
      (uiop:run-program (list "make" "run"))
      (summing (/ 5
                  (length (directory (make-pathname :directory
                                                    (list :absolute
                                                          (namestring
                                                           (car (directory (make-pathname :directory
                                                                                          (list :absolute
                                                                                                *game-runner-dir*
                                                                                                "match-logs")
                                                                                          :name :wild
                                                                                          :type :wild)))))
                                                    :name :wild
                                                    :type :wild))))))))

(defun write-config (optimisation-vector)
  "Write OPTIMISATION-VECTOR to the config file for the bot being optimised."
  (with-open-file (f (make-pathname :directory
                                    (list :absolute
                                          *directory-of-bot-to-optimise*)
                                    :name "score-config")
                     :if-exists :supersede
                     :if-does-not-exist :create
                     :direction :output)
    (format f "'~a" optimisation-vector)))
