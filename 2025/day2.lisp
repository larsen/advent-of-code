(in-package #:advent-of-code)

(defun read-gift-shop-ranges ()
  (let ((raw (read-input-file "inputs/2025/day2")))
    (loop for r in (split-sequence #\, raw)
          collect (mapcar #'parse-integer
                          (split-sequence #\- r)))))

(defun is-sequence-invalid (n)
  (let* ((digits (digits n))
         (l (length digits)))
    (and (evenp (length digits))
         (equal (subseq digits 0 (/ l 2))
                (subseq digits (/ l 2))))))

(defun aoc2025/day2/solution1 ()
  (let ((ranges (read-gift-shop-ranges)))
    (loop for (f s) in ranges
          sum (loop for i from f to s
                   when (is-sequence-invalid i)
                     sum i))))

(defun is-sequence-invalid2 (n)
  (let* ((digits (digits n))
         (l (length digits))
         (ds (divisors l)))
    (loop for d in ds
          for ps = (partition digits (/ l d))
          when (and (> (length ps) 1) ;; Avoid dividing the digits into a single group
                    (= 1 (length (unique ps :test #'equal))))
            do (return T))))

(defun aoc2025/day2/solution2 ()
  (let ((ranges (read-gift-shop-ranges)))
    (loop for (f s) in ranges
          sum (loop for i from f to s
                    when (is-sequence-invalid2 i)
                      sum i))))
