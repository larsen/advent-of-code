(in-package #:advent-of-code)

(defun read-pair-assignments ()
  (loop for l in (uiop:read-file-lines
                  (asdf:system-relative-pathname :advent-of-code "inputs/2022/day4"))
        collect (register-groups-bind ((#'parse-integer interval1-min
                                                        interval1-max
                                                        interval2-min
                                                        interval2-max))
                    ("(\\d+)-(\\d+),(\\d+)-(\\d+)" l)
                  (list (cons interval1-min
                              interval1-max)
                        (cons interval2-min
                              interval2-max)))))

(defun full-overlap (interval1 interval2)
  (labels ((%full-overlap (i1 i2)
             (and (>= (car i1) (car i2))
                  (<= (cdr i1) (cdr i2)))))
    (or (%full-overlap interval1 interval2)
        (%full-overlap interval2 interval1))))

(defun simple-overlap (interval1 interval2)
   (labels ((%simple-overlap (i1 i2)
              (or (>= (car i2) (car i1) (cdr i2))
                  (<= (car i2) (cdr i1) (cdr i2)))))
     (or (%simple-overlap interval1 interval2)
         (%simple-overlap interval2 interval1))))

(defun aoc2022/day4/solution1 ()
  (loop for (interval1 interval2) in (read-pair-assignments)
        count (full-overlap interval1 interval2)))

(defun aoc2022/day4/solution2 ()
  (loop for (interval1 interval2) in (read-pair-assignments)
        count (simple-overlap interval1 interval2)))
