(in-package #:advent-of-code)

(defun read-module-mass-list ()
  (mapcar #'parse-integer
          (uiop:read-file-lines
           (asdf:system-relative-pathname 'advent-of-code "inputs/2019/day1"))))

(defun fuel-required-for-mass (mass)
  (- (floor (/ mass 3)) 2))

(defun aoc2019/day1/solution1 ()
  (reduce #'+ (mapcar #'fuel-required-for-mass (read-module-mass-list))))

(defun iter-fuel-required (mass)
  (let ((fuel-required (fuel-required-for-mass mass)))
    (if (<= fuel-required 0)
        0
        (+ fuel-required
           (iter-fuel-required fuel-required)))))

(defun aoc2019/day1/solution2 ()
  (reduce #'+ (mapcar #'iter-fuel-required (read-module-mass-list))))
