(in-package #:advent-of-code)

(defun read-wires ()
  (loop for line in (uiop:read-file-lines
                     (asdf:system-relative-pathname 'advent-of-code
                                                    "inputs/2019/day3"))
        collect (split-sequence #\, line)))

(defun wire->segments (wire)
  "Given a 'wire', represented as a list of instructions for following
a path, returns a list of segments the path is constitued of.  Each
segment is represented by a quadruple, indicating where it starts and
when it finishes"
  (let ((pos-x 0)
        (pos-y 0)
        (steps 0))
    (loop for instruction in wire
          collect (register-groups-bind (direction (#'parse-integer distance))
                      ("\(.\)\(\\d+\)" instruction)
                    (incf steps distance)
                    (switch (direction :test #'string=)
                      ("L" (list :horizontal steps
                                 pos-x pos-y
                                 (setf pos-x (- pos-x distance)) pos-y))
                      ("R" (list :horizontal steps
                                 pos-x pos-y
                                 (setf pos-x (+ pos-x distance)) pos-y))
                      ("D" (list :vertical steps
                                 pos-x pos-y
                                 pos-x (setf pos-y (- pos-y distance))))
                      ("U" (list :vertical steps
                                 pos-x pos-y
                                 pos-x (setf pos-y (+ pos-y distance)))))))))

(defun intersect-point (segment1 segment2 &key considering-steps)
  (labels ((generate-intermediate-points (segment)
             (destructuring-bind (direction distance x1 y1 x2 y2) segment
               (case direction
                 (:horizontal (loop for x from (min x1 x2) to (max x1 x2)
                                    for steps from distance
                                    collect (list x y1 steps)))
                 (:vertical (loop for y from (min y1 y2) to (max y1 y2)
                                  for steps from distance
                                  collect (list x1 y steps)))))))
    (let ((i (first (intersection (generate-intermediate-points segment1)
                                  (generate-intermediate-points segment2)
                                  :test (lambda (e1 e2)
                                          (and (= (first e1) (first e2))
                                               (= (second e1) (second e2))))))))
      (if i
          (if considering-steps
              (+ (third i) (third i))
              (+ (abs (first i)) (abs (second i))))
          0))))

(defun aoc2019/day3/solution1 ()
  (let ((wires (read-wires)))
    (apply #'min (remove-if #'zerop
                            (map-product #'intersect-point
                                         (wire->segments (first wires))
                                         (wire->segments (second wires)))))))

(defun aoc2019/day3/solution2 ()
    (let ((wires (read-wires)))
    (apply #'min (remove-if #'zerop
                            (map-product (lambda (s1 s2)
                                           (intersect-point s1 s2 :considering-steps t))
                                         (wire->segments (first wires))
                                         (wire->segments (second wires)))))))
