(in-package #:advent-of-code)

(defun read-boarding-passes ()
  (uiop:read-file-lines (asdf:system-relative-pathname 'advent-of-code "inputs/2020/day5")))

(defun seat (boarding-pass)
  (let ((min-row 0)
        (max-row 127)
        (min-col 0)
        (max-col 7))
    (loop for c across boarding-pass
          do (let ((mid-row (ceiling (- max-row min-row) 2))
                   (mid-col (ceiling (- max-col min-col) 2)))
               (case c
                 (#\F (decf max-row mid-row))
                 (#\B (incf min-row mid-row))
                 (#\L (decf max-col mid-col))
                 (#\R (incf min-col mid-col)))))
    (+ (* 8 min-row) min-col)))

(defun aoc2020/day5/solution1 ()
  (loop for bp in (read-boarding-passes)
        maximize (seat bp) into max-seat
        finally (return max-seat)))

(defun aoc2020/day5/solution2 ()
  (let ((seat-ordered-list (sort (mapcar #'seat (read-boarding-passes)) #'>=)))
    (+ 1 (car (first
               (sort (loop for s in seat-ordered-list
                           for idx from 1
                           collect (cons s (- s
                                              (if (> idx 1)
                                                  (nth (- idx 2) seat-ordered-list)
                                                  0))))
                           #'< :key #'cdr))))))
