(in-package #:advent-of-code)

(defun read-gift-size-list ()
  (uiop:read-file-lines (asdf:system-relative-pathname 'advent-of-code
                                                       "inputs/2015/day2")))

(defun box-dimensions (box)
  (register-groups-bind ((#'parse-integer w)
                         (#'parse-integer h)
                         (#'parse-integer l))
            ("(\\d+)x(\\d+)x(\\d+)" box)
    (list w h l)))

(defun aoc2015/day2/solution1 ()
  (let ((wrapping-paper-size 0))
    (loop for box in (read-gift-size-list)
          do (destructuring-bind (w h l)
                 (box-dimensions box)
               (let ((side1 (* l w))
                     (side2 (* w h))
                     (side3 (* h l)))
                 (incf wrapping-paper-size
                       (+ (* 2 side1)
                          (* 2 side2)
                          (* 2 side3)
                          (min side1 side2 side3)))))
          finally (return wrapping-paper-size))))

(defun aoc2015/day2/solution2 ()
  (let ((ribbon-length 0))
    (loop for box in (read-gift-size-list)
          do (destructuring-bind (w h l)
                 (box-dimensions box)
               (let ((volume (* w h l))
                     (perimeter1 (+ w w h h))
                     (perimeter2 (+ h h l l))
                     (perimeter3 (+ l l w w)))
                 (incf ribbon-length
                       (+ volume
                          (min perimeter1 perimeter2 perimeter3)))))
          finally (return ribbon-length))))
