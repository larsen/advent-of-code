(in-package #:advent-of-code)

(defun unique (sequence)
  (let ((seen (make-hash-table)))
    (loop for e in sequence
          do (incf (gethash e seen 0))
          finally (return (hash-table-keys seen)))))

(defun divisors (n)
  "Returns the (unsorted) list of divisors of N"
  (declare (type (integer 0) n))
  (let ((first-half (loop for i from 1 to (isqrt n)
                          when (zerop (rem n i))
                            collect i)))
    (unique (append first-half
                     (mapcar (lambda (d)
                               (/ n d))
                             first-half )))))

(defun aoc2015/day20/solution1 ()
  (loop for n from 1 below 1000000
        for presents-count = (* 10 (apply #'+ (divisors n)))
        when (> presents-count 36000000)
          return n))
