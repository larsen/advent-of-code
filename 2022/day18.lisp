(in-package #:advent-of-code)

(defun read-droplet-cubes ()
  (mapcar (lambda (str)
            (mapcar #'parse-integer (split "," str)))
          (uiop:read-file-lines
           (asdf:system-relative-pathname :advent-of-code "inputs/2022/day18"))))

(defun aoc2022/day18/solution1 ()
  )

(defun aoc2022/day18/solution2 ()
  )
