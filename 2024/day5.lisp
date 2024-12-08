(in-package #:advent-of-code)

(defun read-safety-manual-updates ()
  (destructuring-bind (raw-ordering-rules raw-updates)
      (split "\\n\\n"
             (read-input-file "inputs/2024/day5"))
    (let ((lt-ordering-rules (make-hash-table)))
      (loop for l in (split "\\n" raw-ordering-rules)
            for (n1 n2) = (mapcar #'parse-integer (split "\\|" l))
            do (push n2 (gethash n1 lt-ordering-rules '())))
      (values lt-ordering-rules
              (loop for l in (split "\\n" raw-updates)
                    collect (mapcar #'parse-integer (split "," l)))))))

(defun page-in-the-middle (update)
  (nth (floor (/ (length update) 2))  update))

(defun aoc2024/day5/solution1 ()
  (multiple-value-bind (lt-ordering-rules
                        updates)
      (read-safety-manual-updates)
    (loop with acc = 0
          for u in updates
          when (equal u (stable-sort (copy-list u)
                                     (lambda (a b)
                                       (member b (gethash a lt-ordering-rules)))))
            do (incf acc (page-in-the-middle u))
          finally (return acc))))

(defun aoc2024/day5/solution2 ()
  (multiple-value-bind (lt-ordering-rules
                        updates)
      (read-safety-manual-updates)
    (labels ((page-before (a b)
               (member b (gethash a lt-ordering-rules))))
      (loop with acc = 0
            for u in updates
            for ordered-u = (stable-sort (copy-list u) #'page-before)
            when (not (equal u ordered-u))
              do (incf acc (page-in-the-middle ordered-u))
            finally (return acc)))))
