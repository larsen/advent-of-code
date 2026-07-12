(in-package #:advent-of-code)

(defun read-gift-shop-ranges ()
  (let ((raw (read-input-file "inputs/2025/day2")))
    (loop for r in (split-sequence #\, raw)
          collect (mapcar #'parse-integer
                          (split-sequence #\- r)))))

(defun aoc2025/day2/solution1 ()
  (let ((ranges (read-gift-shop-ranges)))
    (loop for (f s) in ranges
          sum (- s f))))

(defun aoc2025/day2/solution2 ()
  )
