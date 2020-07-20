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
  (progn
    (seed-results-file)
    (iter-optimise)))

(defun re-seed ()
  "Re-compute the score of each member of the current generation."
  (bind ((current-generation (apply #'vector (read-current-generation)))
         (population-size    (length current-generation)))
    (iter
      (for i-idx from 0 below population-size)
      (format t "==========[~a/~a]==========~%" i-idx population-size)
      (for (current-score . i-vector) = (aref current-generation i-idx))
      (for new-score  = (fitness i-vector))
      (format t "Old score: ~a, New Score: ~a~%" current-score new-score)
      (setf (aref current-generation i-idx) (cons new-score i-vector)))
      (write-generation (map 'list #'identity current-generation))
    (format t "Average score for new generation: ~a~%~%" (/ (apply #'+ (map 'list #'car current-generation)) population-size))))

(defun iter-optimise ()
  "Iteratively improve on the current generation."
  (iter
    (for i from 0 below generations)
    (evolve-one-generation)))

(defun seed-results-file ()
  "Seed a results file in the target bot directory."
  (bind ((individuals (seed-population))
         (scores      (mapcar #'fitness individuals))
         (zipped      (mapcar #'cons scores individuals)))
    (write-generation zipped)))

(defconstant f-coefficient (/ 8 10)
  "Controls how much b and c contribute to the new coefficient.")

(defconstant cr-coefficient (/ 9 10)
  "The chance that we cross-over at a given value.")

(defun evolve-one-generation ()
  "Use the current results file to evolve a new generation and write the new file there."
  (bind ((previous-generation (read-current-generation))
         (next-generation     (read-next-generation))
         (population-size     (length previous-generation))
         (current-generation  (apply #'vector
                                     (combine-generations next-generation
                                                          previous-generation
                                                          population-size))))
    (format t "Working on a new generation~%")
    (with-open-file (next-generation-stream (make-pathname :directory
                                                           (list :absolute
                                                                 *directory-of-bot-to-optimise*)
                                                           :name "next-generation")
                                            :if-exists :supersede
                                            :if-does-not-exist :create
                                            :direction :output)
      (iter
        (for i-idx from (length next-generation) below population-size)
        (format t "==========[~a/~a]==========~%" i-idx population-size)
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
        (if (< new-score current-score)
            (progn (setf (aref current-generation i-idx) (cons new-score new-vector))
                   (format next-generation-stream "'~A~%" (cons new-score new-vector)))
            (format next-generation-stream "'~A~%" (cons current-score i-vector)))
        (finish-output next-generation-stream)))
    (write-generation (map 'list #'identity current-generation))
    (format t "Average score for new generation: ~a~%~%" (/ (apply #'+ (map 'list #'car current-generation)) population-size))))

(defun combine-generations (next-generation previous-generation population-size)
  "Produce a current, working generation.

It contains NEXT-GENERATION, then the PREVIOUS-GENERATION filling in
up to POPULATION-SIZE."
  (bind ((working-generation (concatenate 'list
                                          next-generation
                                          (subseq previous-generation (length next-generation)))))
    (assert (eq (length working-generation) population-size))
    working-generation))

(defun write-generation (generation)
  "Write the vector GENERATION to the results file."
  (with-open-file (f (make-pathname :directory
                                    (list :absolute
                                          *directory-of-bot-to-optimise*)
                                    :name "current-generation")
                     :if-exists :supersede
                     :if-does-not-exist :create
                     :direction :output)
    (iter
      (for member in generation)
      (format f "'~A~%" member))))

(defvar *negative-coefficient-start* 4
  "The start of negative coefficients in the optimisation vector.")

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
        (collecting (if (< j *negative-coefficient-start*)
                        (abs (+ a (* f-coefficient (- b c))))
                        (- 0 (abs (+ a (* f-coefficient (- b c)))))))
        (collecting i))))

(defun read-next-generation ()
  "Read the next generation from the results file."
  (ignore-errors
   (with-open-file (f (make-pathname :directory
                                     (list :absolute
                                           *directory-of-bot-to-optimise*)
                                     :name "next-generation"))
     (iter
       (for next-member = (eval (ignore-errors (read f))))
       (while next-member)
       (collecting next-member)))))

(defun read-current-generation ()
  "Read the current generation from the results file."
  (with-open-file (f (make-pathname :directory
                                    (list :absolute
                                          *directory-of-bot-to-optimise*)
                                    :name "current-generation"))
    (iter
      (for next-member = (eval (ignore-errors (read f))))
      (while next-member)
      (collecting next-member))))

(defun seed-population ()
  "Create a population of vectors for coefficients for the global scare heuristic."
  (iter
    (for i from 0 below population-size)
    (collecting (list (/ (random 1000) 1000)
                      (/ (random 1000) 1000)
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
