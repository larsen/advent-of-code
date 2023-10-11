(in-package #:advent-of-code)

(defun read-report ()
  (mapcar #'parse-integer
          (uiop:read-file-lines
           (asdf:system-relative-pathname 'advent-of-code "inputs/2020/day1"))))

(defun aoc2020/day1/solution1 ()
  (let ((result nil))
    (alexandria:map-combinations
     (lambda (lst)
       (if (= 2020 (apply #'+ lst))
           (setf result (apply #'* lst))))
     (read-report) :length 2)
    result))

(defun aoc2020/day1/solution2 ()
  (let ((result nil))
    (alexandria:map-combinations
     (lambda (lst)
       (if (= 2020 (apply #'+ lst))
           (setf result (apply #'* lst))))
     (read-report) :length 3)
    result))
