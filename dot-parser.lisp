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
        (if (string-equal "gini" feature)
          (ppcre:register-groups-bind (class) ("class = y\\[([01])\\]" line)
            (setf (gethash (read-from-string node) leaves)
                  (string-equal class "1")))
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
             (bind ((next-edges (gethash current-node edges)))
               (aif (gethash current-node weights)
                    (cons it (mapcar #'to-tree next-edges))
                    (gethash current-node leaves)))))
    (to-tree 0)))
