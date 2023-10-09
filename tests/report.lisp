(in-package #:advent-of-code/test)

(defclass aoc-report (summary)
  ())

(defun print-table (lst &optional (stream *standard-output*))
  "Prints a list of lists as a table"
  (mapcar (lambda (l)
         (format stream "~&~{~2a~}" l))
       lst))

(defmethod summarize ((report aoc-report))
  (let ((test-hierarchy (make-hash-table)))
    ;; Transform the results into a tree
    (loop for r in (results-with-status :passed report)
          for expr = (expression r)
          when (typep r 'test-result)
            do (if (null (parent expr))
                   (setf (gethash (name expr) test-hierarchy)
                         '())
                   (push (results r)
                         (gethash (name (parent expr))
                                  test-hierarchy))))
    (print-table (loop for k being the hash-keys of test-hierarchy
                       for v being the hash-values of test-hierarchy
                       collect (append (list k)
                                       (mapcar
                                        (lambda (lst)
                                          (if (> (length lst) 1)
                                              "●"
                                              "◐"))
                                        v)))))
  report)

;; ●◐

(defun run-aoc-tests ()
  (parachute:test :advent-of-code/test :report 'aoc-report))
