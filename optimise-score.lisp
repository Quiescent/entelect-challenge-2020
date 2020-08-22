(ql:quickload :metabang-bind)
(ql:quickload :inferior-shell)
(ql:quickload :cl-ppcre)
(ql:quickload :arrow-macros)
(ql:quickload :iterate)
(ql:quickload :bordeaux-threads)

(defpackage optimise-bot
  (:use :cl :iterate :metabang-bind :arrow-macros))

(in-package :optimise-bot)

(defvar population-size 50
  "The number of solutions in the population.")

(defconstant generations 10
  "The number of generations to evolve before cutting off.")

(defvar *output-directory* "/home/edward/wip/entelect-challenge-2020/"
  "The directory into which we write generations.")

;; NOTE: You must setup the bots to run as do-nothing and to-optimise!!!
(defun optimise ()
  "Use differential evolution to find good score weights for the bot."
  (progn
    (seed-results-file)
    (iter-optimise)))

(defun iter-optimise ()
  "Iteratively improve on the current generation."
  (iter
    (for i from 0 below generations)
    (evolve-one-generation)))

(defun seed-results-file ()
  "Seed a results file in the target bot directory."
  (bind ((individuals (seed-population))
         (bot-dir     "/home/edward/wip/entelect-challenge-bots-2020/edward-1")
         (runner-dir  "/home/edward/wip/entelect-challenge-bots-2020/edward-1")
         (scores      (mapcar (lambda (vector) (fitness vector runner-dir bot-dir)) individuals))
         (zipped      (mapcar #'cons scores individuals)))
    (write-generation zipped)))

(defconstant f-coefficient (/ 8 10)
  "Controls how much b and c contribute to the new coefficient.")

(defconstant cr-coefficient (/ 9 10)
  "The chance that we cross-over at a given value.")

(defun evolve-one-generation ()
  "Use the current results file to evolve a new generation and write the new file there."
  (bind ((previous-generation (read-current-generation))
         (chunk-size          (/ (length previous-generation) 5))
         (next-generation     (read-next-generation))
         (population-size     (length previous-generation))
         (current-generation  (apply #'vector
                                     (combine-generations next-generation
                                                          previous-generation
                                                          population-size))))
    (format t "Working on a new generation~%")
    (iter
      (for thread-index from 1 below 6)
      (for thread-bot-dir    = (format nil "/home/edward/wip/entelect-challenge-bots-2020/edward-~a" thread-index))
      (for thread-runner-dir = (format nil "/home/edward/wip/entelect-challenge-bots-2020/runner-~a" thread-index))
      (for thread-start      = (* (1- thread-index) chunk-size))
      (format t "Thread ~a, handling ~a to ~a.~%" thread-index thread-start (+ thread-start chunk-size))
      (format t "Bot dir: ~a~%Runner dir:~a~%" thread-bot-dir thread-runner-dir)
      (collecting
       (bordeaux-threads:make-thread
        (lambda ()
          (bind ((bot-dir    thread-bot-dir)
                 (runner-dir thread-runner-dir)
                 (start      thread-start))
            (with-open-file (next-generation-stream (make-pathname :directory
                                                                   (list :absolute
                                                                         bot-dir)
                                                                   :name (format nil "next-generation-~a" thread-index))
                                                    :if-exists :supersede
                                                    :if-does-not-exist :create
                                                    :direction :output)
              (iter
                (for i-idx from start below (+ start chunk-size))
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
                (for new-score  = (fitness new-vector runner-dir bot-dir))
                (if (> new-score current-score)
                    (progn (setf (aref current-generation i-idx) (cons new-score new-vector))
                           (format next-generation-stream "'~A~%" (cons new-score new-vector)))
                    (format next-generation-stream "'~A~%" (cons current-score i-vector)))
                (finish-output next-generation-stream))))))
       into threads)
      (finally
       (iter
         (for thread in threads)
         (bordeaux-threads:join-thread thread))))
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
                                          *output-directory*)
                                    :name "current-generation")
                     :if-exists :supersede
                     :if-does-not-exist :create
                     :direction :output)
    (iter
      (for member in generation)
      (format f "'~A~%" member))))

(defvar *negative-coefficient-start* 6
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
                                           *output-directory*)
                                     :name "next-generation"))
     (iter
       (for next-member = (eval (ignore-errors (read f))))
       (while next-member)
       (collecting next-member)))))

(defun read-current-generation ()
  "Read the current generation from the results file."
  (with-open-file (f (make-pathname :directory
                                    (list :absolute
                                          *output-directory*)
                                    :name "current-generation"))
    (iter
      (for next-member = (eval (ignore-errors (read f))))
      (while next-member)
      (collecting next-member))))

(defun seed-population ()
  "Create a population of vectors for coefficients for the global scare heuristic."
  (iter
    (for i from 0 below population-size)
    (collecting (list (/ (random 100000) 100000)
                      (/ (random 100000) 100000)
                      (/ (random 100000) 100000)
                      (/ (random 100000) 100000)
                      (/ (random 100000) 100000)
                      (/ (random 100000) 100000)
                      (- 0 (/ (random 100000) 100000))
                      (- 0 (/ (random 100000) 100000))))))

(defvar *fitness-runs* 10
  "The number of times to run the bot to get an average score.")

(defun fitness (optimisation-vector runner-dir bot-dir)
  "Produce a measure of the fitness of OPTIMISATION-VECTOR."
  (progn
    (write-config optimisation-vector bot-dir)
    (iter
      (for i from 0 below *fitness-runs*)
      (format t "[~a/~a]: ~a~%" (1+ i) *fitness-runs* optimisation-vector)
      (uiop:delete-directory-tree (make-pathname :directory
                                                 (list :absolute
                                                       runner-dir
                                                       "match-logs"))
                                  :validate t
                                  :if-does-not-exist :ignore)
      (inferior-shell:run (format nil "cd ~a && make run" runner-dir) :output nil :error-output nil)
      (for margin = (- (final-x (csv-path "A" "Quantum" runner-dir))
                       (final-x (csv-path "B" "LCubed"  runner-dir))))
      (format t "Margin: ~a~%" margin)
      (summing (/ margin *fitness-runs*)))))

(defun final-x (path)
  "Produce the final X position in the CSV file at PATH."
  (with-open-file (file path)
    (iter
      (with line)
      (for next-line = (ignore-errors (read-line file)))
      (while next-line)
      (setf line next-line)
      (finally
       (return
         (->> line
           (cl-ppcre:split ",")
           (nth 3)
           (read-from-string)))))))

(defun csv-path (letter bot-name runner-dir)
  "Produce the CSV file for BOT-NAME in the latest game."
  (format nil
          "~a~a - ~a.csv"
          (->> (directory (make-pathname :directory
                                         (list :absolute
                                               runner-dir
                                               "match-logs")
                                         :name :wild
                                         :type :wild))
            car
            namestring)
          letter
          bot-name))

(defun write-config (optimisation-vector bot-dir)
  "Write OPTIMISATION-VECTOR to the config file for the bot being optimised."
  (with-open-file (f (make-pathname :directory
                                    (list :absolute
                                          bot-dir)
                                    :name "score-config")
                     :if-exists :supersede
                     :if-does-not-exist :create
                     :direction :output)
    (format f "'~a" optimisation-vector)))
