(in-package #:advent-of-code)

(defun read-octopus-grid ()
  (let ((raw-grid (loop for line in (uiop:read-file-lines
                                     (asdf:system-relative-pathname
                                      'advent-of-code "inputs/2021/day11"))
                        collect (mapcar #'parse-integer (split "" line)))))
    (make-array (list 10 10) :initial-contents raw-grid)))

(defun octopus-neighbours (row col grid)
  (loop for (dx dy) in '((-1 -1) (+0 -1) (+1 -1)
                         (-1 +0)         (+1 +0)
                         (-1 +1) (+0 +1) (+1 +1))
        when (handler-case
                 (aref grid (+ row dy) (+ col dx))
               (condition () nil))
          collect (list (+ row dy) (+ col dx))))

(defun octopus-grid-step (octopus-grid)
  (let ((new-octopus-grid (alexandria:copy-array octopus-grid))
        (flashed (alexandria:copy-array octopus-grid))
        (total-flashes 0)
        (flashes 0))

    (map2d #'1+ new-octopus-grid)

    (loop do (setf flashes 0)
             (loop for col from 0 below 10
                   do (loop for row from 0 below 10
                            when (and (> (aref new-octopus-grid row col) 9)
                                      (/= 1 (aref flashed row col)))
                              do (incf total-flashes)
                                 (incf flashes)
                                 (setf (aref flashed row col) 1)
                                 (loop for (n-row n-col) in (octopus-neighbours row col new-octopus-grid)
                                       do (incf (aref new-octopus-grid n-row n-col)))))
          until (zerop flashes))

    (map2d (lambda (elem)
                   (if (> elem 9)
                       0
                       elem)) new-octopus-grid)

    (values new-octopus-grid total-flashes)))

(defun aoc2021/day11/solution1 ()
  (let ((octopus-grid (read-octopus-grid))
        (total-flashes 0))
    (loop repeat 100
          do (multiple-value-bind (new-grid flashes)
                 (octopus-grid-step octopus-grid)
               (incf total-flashes flashes)
               (setf octopus-grid new-grid))
          finally (return total-flashes))))

(defun all-grid-elems (grid)
  (loop with (maxrow maxcol) = (array-dimensions grid)
        for col from 0 below maxcol
        append (loop for row from 0 below maxrow
                     collect (aref grid row col))))

(defun aoc2021/day11/solution2 ()
  (loop for counter from 0
        for octopus-grid = (read-octopus-grid) then (octopus-grid-step octopus-grid)
        until (= 100 (count 0 (all-grid-elems octopus-grid)))
        finally (return counter)))
