(ql:quickload :metabang-bind)
(ql:quickload :iterate)

(defpackage optimise-bot
  (:use :cl :iterate :metabang-bind))

(in-package :optimise-bot)

(defvar population-size 100
  "The number of solutions in the population.")

(defvar *directory-of-bot-to-optimise* "home/edward/wip/entelect-challenge-2020/bots/to-optimise"
  "The directory of the bot being optimised.")

(defvar *game-runner-dir* "/home/edward/wip/EntelectChallenge-2020-Overdrive/game-runner"
  "The directory where the game runner lives.")

(defconstant generations 10
  "The number of generations to evolve before cutting off.")

;; NOTE: You must setup the bots to run as do-nothing and to-optimise!!!
(defun optimise ()
  "Use differential evolution to find good score weights for the bot."
  (iter
    (initially (seed-results-file))
    (for i from 0 below generations)
    (evolve-one-generation)))

(defun seed-results-file ()
  "Seed a results file in the target bot directory."
  (bind ((individuals (seed-population))
         (scores      (mapcar #'fitness individuals))
         (zipped      (mapcar #'cons scores individuals)))
    (write-generation zipped)))

(defconstant f-coefficient 0.8
  "Controls how much b and c contribute to the new coefficient.")

(defconstant cr-coefficient 0.9
  "The chance that we cross-over at a given value.")

(defun evolve-one-generation ()
  "Use the current results file to evolve a new generation and write the new file there."
  (bind ((current-generation (read-current-generation))
         (population-size    (length current-generation)))
    (iter
      (for i-idx from 0 below population-size)
      (for (current-score . i-vector) = (aref current-generation i-idx))
      (for a-idx = (iter
                     (for result = (random population-size))
                     (when (/= result i-idx)
                       (return result))))
      (for (a-score . a-vector) = (aref current-generation a-idx))
      (for b-idx = (iter
                     (for result = (random population-size))
                     (when (and (/= result i-idx)
                                (/= result a-idx))
                       (return result))))
      (for (b-score . b-vector) = (aref current-generation b-idx))
      (for c-idx = (iter
                     (for result = (random population-size))
                     (when (and (/= result i-idx)
                                (/= result a-idx)
                                (/= result b-idx))
                       (return result))))
      (for (c-score . c-vector) = (aref current-generation c-idx))
      (for new-vector = (cross-over i-vector a-vector b-vector c-vector))
      (for new-score  = (fitness new-vector))
      (when (< new-score current-score)
        (setf (aref current-generation i-idx) new-vector)))
    (write-generation current-generation)))

(defun write-generation (generation)
  "Write GENERATION to the results file."
  (with-open-file (f (make-pathname :directory
                                    (list :absolute
                                          *directory-of-bot-to-optimise*)
                                    :name "results")
                     :if-exists :supersede
                     :if-does-not-exist :create
                     :direction :output)
    (format f "'~A" (apply #'vector generation))))

(defun cross-over (i-vector a-vector b-vector c-vector)
  "Produce the result of crossing I-VECTOR with A-VECTOR B-VECTOR and C-VECTOR."
  (iter
    (with vector-length = (length i-vector))
    (for i in i-vector)
    (for a in a-vector)
    (for b in b-vector)
    (for c in c-vector)
    (for j from 0)
    (for crossing-point = (random vector-length))
    (if (or (= j crossing-point)
            (< (random 1.0) cr-coefficient))
        (collecting (+ a (* f-coefficient (- b c))))
        (collecting i))))

(defun read-current-generation ()
  "Read the current generation from the results file."
  (with-open-file (f (make-pathname :directory
                                    (list :absolute
                                          *directory-of-bot-to-optimise*)
                                    :name "results"))
    (eval (read f))))

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
      (summing (/ (length (directory (make-pathname :directory
                                                    (list :absolute
                                                          (namestring
                                                           (car (directory (make-pathname :directory
                                                                                          (list :absolute
                                                                                                *game-runner-dir*
                                                                                                "match-logs")
                                                                                          :name :wild
                                                                                          :type :wild)))))
                                                    :name :wild
                                                    :type :wild)))
                  5)))))

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
