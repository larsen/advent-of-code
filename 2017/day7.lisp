(in-package #:advent-of-code)

(defstruct (circus-tree
            (:print-function))
  (name nil)
  (weight 0)
  (parent nil)
  (children nil)
  (children-names nil))

;; TODO refactor!
(defun extract-number (str)
  (multiple-value-bind (_s matches)
      (ppcre:scan-to-strings "\\((\\d+)\\)" str)
    (declare (ignorable _s))
    (parse-integer (aref matches 0))))

(defun extract-children-list (str)
  (ppcre:split "," str))

;; TODO refactor!
(defun read-recursive-circus ()
  (loop for line in (uiop:read-file-lines (asdf:system-relative-pathname
                                           'advent-of-code "inputs/2017/day7"))
        collect (destructuring-bind (name weight &optional arrow &rest children)
                    (split-sequence #\Space line)
                  (declare (ignorable arrow))
                  (make-circus-tree :name name
                                    :weight (extract-number weight)
                                    :children-names (if children
                                                (extract-children-list
                                                 (apply #'concatenate 'string children))
                                                nil)))))

(defun build-circus-tree ()
  (let ((circus (read-recursive-circus))
        (circus-lookup-table (make-hash-table :test 'equalp)))
    (loop for tower in circus
          do (setf (gethash (circus-tree-name tower) circus-lookup-table)
                   tower))
    (loop for tower in circus
          do (setf (circus-tree-children tower)
                   (loop for c in (circus-tree-children-names tower)
                         do (setf (circus-tree-parent (gethash c circus-lookup-table))
                                  tower)
                         collect (gethash c circus-lookup-table))))
    (values circus circus-lookup-table)))

(defun aoc2017/day7/solution1 ()
  (let ((circus (build-circus-tree)))
    ;; The base is the only node with no parent
    (loop for tower in circus
          when (null (circus-tree-parent tower))
            return (circus-tree-name tower))))

(defun weight-with-children (tower)
  (values
   (+ (circus-tree-weight tower)
      (loop for child in (circus-tree-children tower)
            sum (weight-with-children child)))
   (circus-tree-weight tower)
   (loop for child in (circus-tree-children tower)
         collect (weight-with-children child))))

(defun aoc2017/day7/solution2 ()
  (multiple-value-bind (circus circus-lookup-table)
      (build-circus-tree)
    (declare (ignorable circus))
    (loop for tower-name being the hash-keys of circus-lookup-table
          for tower = (gethash tower-name circus-lookup-table)
          for children = (circus-tree-children tower)
          for children-weights = (mapcar #'weight-with-children children)
          when (and children-weights (not (apply #'= children-weights)))
            do (print tower-name)
               (print (circus-tree-weight tower))
               (print children)
               (print children-weights))
    circus-lookup-table))
