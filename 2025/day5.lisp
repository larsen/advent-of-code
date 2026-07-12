(in-package #:advent-of-code)

(defun read-cafeteria-database ()
  (let ((raw (read-input-file "inputs/2025/day5")))
    (destructuring-bind (ranges ingredients)
        (split "\\n\\n" raw)
      (values (mapcar (lambda (l)
                        (mapcar #'parse-integer (split "-" l)))
                      (split "\\n" ranges))
              (mapcar #'parse-integer (split "\\n" ingredients))))))

(defun aoc2025/day5/solution1 ()
  (multiple-value-bind (ranges ingredients)
      (read-cafeteria-database)
    (loop for i in ingredients
          count (loop for r in ranges
                        thereis (<= (first r) i (second r))))))

(defun aoc2025/day5/solution2 ()
  )
