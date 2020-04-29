(in-package :bot)

(defun dot-file-to-list-tree (relative-file-path)
  "Produce a list-of-lists representation of the dot graph read from a file.

File is located at the relative path RELATIVE-FOLDER-PATH.

This is *not* a full parser.  It does a quick and dirty job to build a
data representation of the tree for my bot.

FIXME: This function is a bit of a hack and relies entirely on the
idea that the dotfile output of scikit is in the order of right
leaning edges."
  (with-open-file (file relative-file-path)
    (iter
      (with edges = (make-hash-table :test #'eq))
      (with weights = (make-hash-table :test #'eq))
      (with leaves = (make-hash-table :test #'eq))
      (with line)
      (setf line (read-line file nil))
      (while line)
      (ppcre:register-groups-bind (start term) ("^([0-9]+) -> ([0-9]+)" line)
        (bind ((beg      (read-from-string start))
               (end      (read-from-string term))
               (existing (gethash beg edges)))
          (setf (gethash beg edges) (nconc existing (list end)))))
      (ppcre:register-groups-bind (node feature operator value)
          ("^([0-9]+) \\[label=\"([_a-zA-Z]+) ([<>=][<>=]?) ([0-9.]+)" line)
        (if (string-equal "entropy" feature)
          (ppcre:register-groups-bind (class) ("class = ([A-Z]+)" line)
            (setf (gethash (read-from-string node) leaves)
                  (intern class)))
          (setf (gethash (read-from-string node) weights)
                `(,(intern operator) ,(intern (string-upcase feature)) ,(read-from-string value)))))
      (finally (return (edges-and-weights-to-tree edges weights leaves))))))

(defun edges-and-weights-to-tree (edges weights leaves)
  "Produce a lisp tree of lists corresponding to a decision tree for my bot.

EDGES define directions which we can go in, ordering is significant.
At most two edges are expected for each node.  The first one is the
direction to take when the condition is true.

WEIGHTS are expressions to evaluation to go left or right at a
branch.

LEAVES are the labels of a leaf node, if we hit one.  i.e. T if it was
a good move."
  (labels ((to-tree (current-node)
             (when current-node
               (bind ((next-edges (gethash current-node edges))
                      (weight     (gethash current-node weights))
                      (true-edge  (car next-edges))
                      (true-leaf  (gethash true-edge leaves))
                      (false-edge (cadr next-edges))
                      (false-leaf (gethash false-edge leaves)))
                 (list t
                       (list weight (or true-leaf  (to-tree true-edge)))
                       (if false-leaf
                           (list t false-leaf)
                           (to-tree false-edge)))))))
    (to-tree 0)))

;; this is the problem!!!
(defun raise-tripple-lists (xs)
  "Turn (((stuff...))) nesting into ((stuff..)) nesting."
  (if (and (consp xs)
           (consp (car xs))
           (consp (caar xs)))
      (cons (car xs)
            (mapcar #'raise-tripple-lists (cdr xs)))
      xs))

#+nil
'((<= SPEED 12.0)
  ((<= MOVE 2.5)
   ((<= MUD_AHEAD 1.5)
    ((<= SPEED 7.0)
     ((<= SPEED 4.0)
      ((<= MUD_AHEAD 0.5)
       ((<= MOVE 0.5)
        (((<= BOOSTS 0.5) TERRIBLE))
        (((<= BOOSTS 0.5) GREAT)))))))))

;; Failing case
#+nil
'((<= SPEED 12.0)
  ((<= MOVE 2.5)
   ((<= MUD_AHEAD 1.5)
    ((<= SPEED 7.0)
     ((<= SPEED 4.0)
      ((<= MUD_AHEAD 0.5)
       ((<= MOVE 0.5)
        (((<= BOOSTS 0.5) TERRIBLE))
        (((<= BOOSTS 0.5) GREAT)))
       ((<= SPEED_AHEAD 0.5)
        (((<= MOVE 0.5) GREAT)
         (((<= BOOSTS 0.5) GREAT)))))
      ((<= MUD_AHEAD 0.5)
       ((<= SPEED 5.5)
        (((<= MUD_UP 2.5) BAD))
        ((<= SPEED_AHEAD 0.5)
         (((<= MOVE 0.5) GREAT))))
       ((<= BOOSTS 0.5)
        ((<= SPEED_AHEAD 0.5)
         ((<= MOVE 0.5)
          (((<= SPEED 5.5) GREAT))
          ((<= MUD_DOWN 0.5)
           ((<= SPEED_DOWN 0.5)
            ((<= MOVE 1.5)
             (((<= SPEED_UP 0.5) BAD))))
           ((<= MUD_UP 0.5)
            ((<= SPEED_UP 0.5)
             (((<= MOVE 1.5) BAD)))))))
        ((<= SPEED_AHEAD 0.5)
         (((<= BOOSTS 1.5) TERRIBLE))))))))))


#+nil
'(T
 ((<= SPEED 12.0)
  (T
   ((<= MOVE 2.5)
    (T
     ((<= MUD_AHEAD 1.5)
      (T
       ((<= SPEED 7.0)
        (T
         ((<= SPEED 4.0)
          (T
           ((<= MUD_AHEAD 0.5)
            (T
             ((<= MOVE 0.5)
              (T ((<= BOOSTS 0.5) TERRIBLE)
                 (T GREAT)))
             (T ((<= BOOSTS 0.5) GREAT) (T TERRIBLE))))
           (T
            ((<= SPEED_AHEAD 0.5)
             (T ((<= MOVE 0.5) GREAT)
              (T ((<= BOOSTS 0.5) GREAT) (T TERRIBLE))))
            (T TERRIBLE))))
         (T
          ((<= MUD_AHEAD 0.5)
           (T ((<= SPEED 5.5) (T ((<= MUD_UP 2.5) BAD) (T BAD)))
            (T ((<= SPEED_AHEAD 0.5) (T ((<= MOVE 0.5) GREAT) (T BAD)))
             (T GREAT))))
          (T
           ((<= BOOSTS 0.5)
            (T
             ((<= SPEED_AHEAD 0.5)
              (T ((<= MOVE 0.5) (T ((<= SPEED 5.5) GREAT) (T BAD)))
               (T
                ((<= MUD_DOWN 0.5)
                 (T
                  ((<= SPEED_DOWN 0.5)
                   (T ((<= MOVE 1.5) (T ((<= SPEED_UP 0.5) BAD) (T BAD)))
                    (T BAD)))
                  (T GREAT)))
                (T
                 ((<= MUD_UP 0.5)
                  (T ((<= SPEED_UP 0.5) (T ((<= MOVE 1.5) BAD) (T BAD)))
                   (T GREAT)))
                 (T BAD)))))
             (T BAD)))
           (T ((<= SPEED_AHEAD 0.5) (T ((<= BOOSTS 1.5) TERRIBLE) (T BAD)))
            (T BAD))))))
       (T
        ((<= BOOSTS 0.5)
         (T
          ((<= MOVE 0.5)
           (T
            ((<= SPEED_AHEAD 0.5)
             (T ((<= SPEED 8.5) (T ((<= MUD_AHEAD 0.5) BAD) (T BAD)))
              (T ((<= MUD_AHEAD 0.5) BAD) (T BAD))))
            (T ((<= SPEED 8.5) BAD) (T ((<= MUD_AHEAD 0.5) BAD) (T BAD)))))
          (T
           ((<= MUD_UP 0.5)
            (T
             ((<= MOVE 1.5)
              (T ((<= SPEED_UP 0.5) (T ((<= SPEED 8.5) BAD) (T BAD)))
               (T ((<= SPEED 8.5) BAD) (T BAD))))
             (T
              ((<= MUD_DOWN 0.5)
               (T ((<= SPEED_DOWN 0.5) BAD) (T ((<= SPEED 8.5) BAD) (T BAD))))
              (T ((<= MUD_DOWN 1.5) BAD)
               (T ((<= SPEED_DOWN 0.5) GOOD) (T GREAT))))))
           (T
            ((<= MUD_DOWN 0.5)
             (T
              ((<= MOVE 1.5)
               (T ((<= MUD_UP 1.5) BAD) (T ((<= SPEED 8.5) GREAT) (T GOOD))))
              (T ((<= SPEED_DOWN 0.5) BAD) (T ((<= SPEED 8.5) BAD) (T BAD)))))
            (T
             ((<= MUD_UP 1.5)
              (T
               ((<= MOVE 1.5)
                (T ((<= SPEED 8.5) BAD) (T ((<= SPEED_UP 0.5) BAD) (T BAD))))
               (T ((<= MUD_DOWN 1.5) BAD) (T GREAT))))
             (T
              ((<= MUD_DOWN 1.5)
               (T ((<= MOVE 1.5) (T ((<= SPEED_UP 0.5) GOOD) (T GREAT)))
                (T ((<= SPEED 8.5) BAD) (T BAD))))
              (T ((<= SPEED 8.5) GREAT) (T GOOD))))))))
        (T
         ((<= SPEED 8.5)
          (T
           ((<= MOVE 0.5)
            (T ((<= MUD_AHEAD 0.5) (T ((<= SPEED_AHEAD 0.5) BAD) (T BAD)))
             (T ((<= MUD_UP 1.5) BAD) (T BAD))))
           (T
            ((<= MUD_DOWN 0.5)
             (T
              ((<= MOVE 1.5)
               (T ((<= MUD_UP 0.5) TERRIBLE)
                (T ((<= MUD_UP 1.5) GREAT) (T BAD))))
              (T TERRIBLE)))
            (T
             ((<= MUD_UP 0.5)
              (T ((<= MOVE 1.5) (T ((<= BOOSTS 1.5) TERRIBLE) (T BAD)))
               (T ((<= MUD_DOWN 1.5) GREAT) (T BAD))))
             (T ((<= MUD_DOWN 1.5) GREAT)
              (T ((<= MUD_UP 1.5) (T ((<= MOVE 1.5) GREAT) (T BAD)))
               (T BAD)))))))
         (T
          ((<= MOVE 0.5)
           (T ((<= MUD_AHEAD 0.5) (T ((<= SPEED_AHEAD 0.5) BAD) (T BAD)))
            (T ((<= SPEED_AHEAD 0.5) GREAT) (T BAD))))
          (T
           ((<= MUD_DOWN 0.5)
            (T
             ((<= MOVE 1.5)
              (T ((<= MUD_UP 0.5) TERRIBLE)
               (T ((<= MUD_UP 2.5) (T ((<= MUD_UP 1.5) GREAT) (T GREAT)))
                (T BAD))))
             (T ((<= SPEED_DOWN 0.5) TERRIBLE) (T BAD))))
           (T
            ((<= MUD_UP 0.5)
             (T ((<= MOVE 1.5) (T ((<= SPEED_UP 0.5) TERRIBLE) (T BAD)))
              (T ((<= MUD_DOWN 2.5) (T ((<= MUD_DOWN 1.5) GREAT) (T GREAT)))
               (T BAD))))
            (T
             ((<= MUD_DOWN 2.5)
              (T
               ((<= MUD_DOWN 1.5)
                (T
                 ((<= MOVE 1.5)
                  (T ((<= MUD_UP 2.5) (T ((<= MUD_UP 1.5) GREAT) (T GREAT)))
                   (T BAD)))
                 (T GREAT)))
               (T
                ((<= MOVE 1.5)
                 (T ((<= MUD_UP 2.5) (T ((<= MUD_UP 1.5) GREAT) (T GREAT)))
                  (T BAD)))
                (T GREAT))))
             (T
              ((<= MOVE 1.5)
               (T ((<= MUD_UP 2.5) (T ((<= MUD_UP 1.5) GREAT) (T GREAT)))
                (T BAD)))
              (T BAD))))))))))
     (T
      ((<= MOVE 0.5)
       (T
        ((<= SPEED 8.5)
         (T
          ((<= SPEED 7.0)
           (T
            ((<= BOOSTS 0.5)
             (T ((<= SPEED_AHEAD 0.5) GREAT)
              (T ((<= SPEED 4.0) TERRIBLE) (T BAD))))
            (T ((<= SPEED 4.5) GREAT) (T BAD))))
          (T
           ((<= MUD_AHEAD 2.5)
            (T
             ((<= SPEED_AHEAD 0.5)
              (T ((<= MUD_DOWN 0.5) (T ((<= BOOSTS 0.5) BAD) (T GREAT)))
               (T BAD)))
             (T ((<= SPEED_AHEAD 1.5) GREAT) (T BAD))))
           (T ((<= BOOSTS 0.5) (T ((<= SPEED_AHEAD 0.5) GOOD) (T GREAT)))
            (T GREAT)))))
        (T ((<= MUD_AHEAD 2.5) (T ((<= BOOSTS 0.5) GREAT) (T GREAT)))
         (T ((<= BOOSTS 0.5) (T ((<= SPEED_AHEAD 0.5) GOOD) (T GOOD)))
          (T GOOD)))))
      (T
       ((<= BOOSTS 0.5)
        (T
         ((<= MUD_DOWN 0.5)
          (T
           ((<= SPEED_DOWN 0.5)
            (T
             ((<= MOVE 1.5)
              (T
               ((<= MUD_UP 1.5)
                (T ((<= MUD_UP 0.5) (T ((<= SPEED_UP 0.5) BAD) (T BAD)))
                 (T BAD)))
               (T ((<= SPEED 8.5) GOOD)
                (T ((<= MUD_AHEAD 2.5) GOOD) (T GREAT)))))
             (T ((<= SPEED 8.5) BAD) (T BAD))))
           (T ((<= SPEED 7.0) GREAT) (T BAD))))
         (T
          ((<= MUD_UP 1.5)
           (T
            ((<= SPEED_UP 0.5)
             (T
              ((<= MOVE 1.5)
               (T ((<= MUD_UP 0.5) (T ((<= SPEED 8.5) BAD) (T BAD)))
                (T ((<= SPEED 8.5) (T ((<= SPEED 7.0) GREAT) (T BAD)))
                 (T BAD))))
              (T
               ((<= MUD_DOWN 1.5)
                (T ((<= SPEED 8.5) BAD) (T ((<= SPEED_DOWN 0.5) BAD) (T BAD))))
               (T ((<= MUD_DOWN 2.5) GREAT) (T GOOD)))))
            (T ((<= MUD_UP 0.5) BAD) (T BAD))))
          (T
           ((<= MUD_DOWN 1.5)
            (T ((<= SPEED 8.5) GREAT)
             (T ((<= MOVE 1.5) GREAT) (T ((<= SPEED_DOWN 0.5) BAD) (T BAD)))))
           (T GOOD)))))
       (T
        ((<= MUD_DOWN 0.5)
         (T ((<= MOVE 1.5) (T ((<= MUD_UP 0.5) TERRIBLE) (T GREAT)))
          (T TERRIBLE)))
        (T
         ((<= MUD_UP 0.5)
          (T ((<= MOVE 1.5) TERRIBLE) (T ((<= SPEED_DOWN 0.5) GREAT) (T BAD))))
         (T GREAT)))))))
   (T
    ((<= MUD_AHEAD 0.5)
     (T
      ((<= SPEED 8.5)
       (T ((<= SPEED 7.0) (T ((<= SPEED 5.5) TERRIBLE) (T TERRIBLE)))
        (T ((<= SPEED_AHEAD 0.5) (T ((<= BOOSTS 1.5) TERRIBLE) (T TERRIBLE)))
         (T TERRIBLE))))
      (T
       ((<= BOOSTS 1.5)
        (T ((<= SPEED_AHEAD 0.5) (T ((<= MUD_UP 2.5) TERRIBLE) (T TERRIBLE)))
         (T TERRIBLE)))
       (T TERRIBLE))))
    (T
     ((<= SPEED 5.5)
      (T ((<= MUD_AHEAD 2.5) (T ((<= MUD_AHEAD 1.5) TERRIBLE) (T TERRIBLE)))
       (T ((<= MUD_AHEAD 3.5) (T ((<= SPEED 4.0) TERRIBLE) (T BAD)))
        (T GREAT))))
     (T
      ((<= SPEED 7.0)
       (T ((<= MUD_AHEAD 1.5) (T ((<= BOOSTS 1.5) GREAT) (T TERRIBLE)))
        (T
         ((<= MUD_AHEAD 2.5)
          (T ((<= SPEED_AHEAD 0.5) (T ((<= BOOSTS 1.5) GREAT) (T GREAT)))
           (T GREAT)))
         (T ((<= MUD_AHEAD 3.5) BAD) (T ((<= MUD_UP 3.5) BAD) (T GREAT))))))
      (T
       ((<= MUD_AHEAD 2.5)
        (T
         ((<= SPEED 8.5)
          (T ((<= MUD_AHEAD 1.5) BAD)
           (T ((<= SPEED_AHEAD 0.5) (T ((<= BOOSTS 1.5) BAD) (T BAD)))
            (T BAD))))
         (T ((<= MUD_AHEAD 1.5) BAD) (T BAD))))
       (T ((<= SPEED 8.5) (T ((<= MUD_AHEAD 3.5) BAD) (T GREAT)))
        (T ((<= MUD_AHEAD 3.5) (T ((<= BOOSTS 2.5) GREAT) (T GREAT)))
         (T GOOD)))))))))
 (T
  ((<= MUD_AHEAD 0.5)
   (T
    ((<= MOVE 0.5)
     (T ((<= SPEED_AHEAD 0.5) (T ((<= BOOSTS 0.5) GOOD) (T GOOD))) (T GOOD)))
    (T
     ((<= MUD_DOWN 0.5)
      (T ((<= MOVE 1.5) (T ((<= MUD_UP 0.5) GOOD) (T OK))) (T GOOD)))
     (T ((<= MUD_UP 0.5) (T ((<= MOVE 1.5) GOOD) (T OK))) (T OK)))))
  (T
   ((<= MOVE 0.5)
    (T
     ((<= BOOSTS 0.5)
      (T ((<= SPEED_AHEAD 0.5) (T ((<= MUD_DOWN 2.305843) OK) (T OK)))
       (T ((<= MUD_AHEAD 3.5) OK) (T OK))))
     (T ((<= MUD_AHEAD 3.5) OK) (T OK))))
   (T
    ((<= MUD_DOWN 0.5)
     (T ((<= MOVE 1.5) (T ((<= MUD_UP 0.5) GOOD) (T OK)))
      (T ((<= SPEED_DOWN 0.5) GOOD) (T GOOD))))
    (T ((<= MUD_UP 0.5) (T ((<= MOVE 1.5) GOOD) (T OK)))
     (T
      ((<= BOOSTS 0.5)
       (T ((<= MUD_AHEAD 2.5) OK)
        (T
         ((<= SPEED_UP 0.5)
          (T ((<= SPEED_DOWN 0.5) OK) (T ((<= MUD_DOWN 3.5) OK) (T OK))))
         (T ((<= MUD_UP 3.5) OK) (T ((<= SPEED_DOWN 0.5) OK) (T OK))))))
      (T OK)))))))
