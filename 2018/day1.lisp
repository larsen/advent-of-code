(in-package #:advent-of-code)

(defun read-frequencies ()
  (mapcar #'parse-integer
          (uiop:read-file-lines
           (asdf:system-relative-pathname :advent-of-code "inputs/2018/day1"))))

(defun aoc2018/day1/solution1 ()
  (reduce #'+ (read-frequencies)))

(defun aoc2018/day1/solution2 ()
  (let* ((frequencies (read-frequencies))
         (len (length frequencies))
         (seen-acc-values (make-hash-table)))
    (loop with acc = 0
          for i from 0
          for f = (nth (mod i len) frequencies)
          do (incf acc f)
          when (gethash acc seen-acc-values)
            return acc
          do (setf (gethash acc seen-acc-values) acc))))
