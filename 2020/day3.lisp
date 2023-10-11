(in-package #:advent-of-code)

(defun read-tree-map ()
  (uiop:read-file-lines
   (asdf:system-relative-pathname 'advent-of-code "inputs/2020/day3")))

(defun check-slope (right down)
  (let* ((tree-map (read-tree-map))
         (map-width (length (first tree-map)))
         (pos-x 0)
         (pos-y 0)
         (trees-encountered 0))
    (loop when (char= (nth pos-x (coerce (nth pos-y tree-map) 'list)) #\#)
            do (incf trees-encountered)
          do (setf pos-x (mod (+ pos-x right) map-width))
             (incf pos-y down)
          while (< pos-y (length tree-map))
          finally (return trees-encountered))))

(defun aoc2020/day3/solution1 ()
  (check-slope 3 1))

(defun aoc2020/day3/solution2 ()
  (* (check-slope 1 1)
     (check-slope 3 1)
     (check-slope 5 1)
     (check-slope 7 1)
     (check-slope 1 2)))
