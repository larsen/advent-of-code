(in-package #:advent-of-code)

(defun read-oasis-report ()
  (let ((file-content (read-input-file-as-lines "inputs/2023/day9")))
    (loop for l in file-content
          collect (mapcar #'parse-integer (split "\\s" l)))))

(defun differentiate (history)
  (loop for (a b) on history
        when b collect (- b a)))

(defun predict-next-value (history)
  (loop for h = history then (differentiate h)
        collect (car (last h)) into res
        until (loop for x in h always (= x 0))
        finally (return (sum res))))

(defun aoc2023/day9/solution1 ()
  (loop for history in (read-oasis-report)
        sum (predict-next-value history)))

(defun aoc2023/day9/solution2 ()
  (loop for history in (read-oasis-report)
        sum (predict-next-value (reverse history))))
