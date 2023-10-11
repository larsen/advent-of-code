(in-package #:advent-of-code)

(defun read-groups-answers ()
  (split "\\n\\n" (uiop:read-file-string
                   (asdf:system-relative-pathname 'advent-of-code "inputs/2020/day6"))))

(defun count-distinct-answers (group-answers)
  (let ((seen-answers (make-hash-table)))
    (loop for a across (remove-if-not #'alpha-char-p group-answers)
          do (incf (gethash a seen-answers 0))
          finally (return (length (alexandria:hash-table-keys seen-answers))))))

(defun count-unanimous-answers (group-answers)
  (let ((seen-answers (make-hash-table))
        (group-size (length (split "\\n" group-answers))))
    (loop for a across (remove-if-not #'alpha-char-p group-answers)
          do (incf (gethash a seen-answers 0))
          finally (return (count-if (lambda (k)
                                      (= group-size (gethash k seen-answers)))
                                    (alexandria:hash-table-keys seen-answers))))))

(defun aoc2020/day6/solution1 ()
  (reduce #'+ (mapcar #'count-distinct-answers (read-groups-answers))))

(defun aoc2020/day6/solution2 ()
  (reduce #'+ (mapcar #'count-unanimous-answers (read-groups-answers))))
