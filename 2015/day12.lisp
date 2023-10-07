(in-package #:advent-of-code)

(defun read-json-file ()
  (jonathan:parse
   (uiop:read-file-string
    (asdf:system-relative-pathname 'advent-of-code
                                   "inputs/2015/day12"))))

(defun traverse-and-grep-struct (struct fn)
  (loop for elem in struct
        if (and (atom elem) (funcall fn elem))
          collect elem
        if (listp elem)
          append (traverse-and-grep-struct elem fn)))

(defun aoc2015/day12/solution1 ()
  (reduce #'+
          (traverse-and-grep-struct
           (read-json-file)
           (lambda (elem)
             (numberp elem)))))



(defun read-json-file-as-hash-table ()
  (jonathan:parse
   (uiop:read-file-string
    (asdf:system-relative-pathname 'advent-of-code
                                   "inputs/2015/day12"))
   :as :hash-table))

(defun traverse-and-grep-hash-table (struct fn traverse-fn)
  (cond
    ((hash-table-p struct)
     (loop for elem being the hash-values of struct
           if (and (atom elem) (funcall fn elem struct))
             collect elem
           if (and (funcall traverse-fn elem struct)
                   (or (listp elem)
                       (hash-table-p elem)))
             append (traverse-and-grep-hash-table elem fn traverse-fn)))
    ((listp struct)
     (loop for elem in struct
           if (and (atom elem) (funcall fn elem struct))
             collect elem
           if (and (funcall traverse-fn elem struct)
                   (or (listp elem)
                       (hash-table-p elem)))
              append (traverse-and-grep-hash-table elem fn traverse-fn)))))

(defun aoc2015/day12/solution2 ()
  (reduce #'+
          (traverse-and-grep-hash-table
           (read-json-file-as-hash-table)
           (lambda (elem struct)
             (and (numberp elem)
                  (or (not (hash-table-p struct))
                      (not (member "red" (hash-table-values struct)
                                   :test #'equal)))))
           (lambda (elem struct)
             (declare (ignorable elem struct))
             (or (not (hash-table-p struct))
                 (not (member "red" (hash-table-values struct)
                              :test #'equal)))))))
