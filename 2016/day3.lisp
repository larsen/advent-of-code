(in-package #:advent-of-code)

(defun parse-triangle-description (triangle-description)
    (register-groups-bind ((#'parse-integer l1)
                           (#'parse-integer l2)
                           (#'parse-integer l3))
        ("(\\d+)\\s+(\\d+)\\s+(\\d+)$" triangle-description)
      (list l1 l2 l3)))

(defun read-triangles ()
  (loop for triangle-description in (read-input-file-as-lines "inputs/2016/day3")
        collect (parse-triangle-description triangle-description)))

(defun is-triangle? (triangle)
  (destructuring-bind (l1 l2 l3) triangle
    (and (> (+ l1 l2) l3)
         (> (+ l2 l3) l1)
         (> (+ l1 l3) l2))))

(defun aoc2016/day3/solution1 ()
  (length (remove-if (lambda (triangle)
                       (not (is-triangle? triangle)))
                     (read-triangles))))

(defun read-triangles-vertically ()
  (loop for (line1 line2 line3) on (read-input-file-as-lines "inputs/2016/day3") by #'cdddr
        for triple1 = (parse-triangle-description line1)
        for triple2 = (parse-triangle-description line2)
        for triple3 = (parse-triangle-description line3)
        ;; Now reshuffling the vertices
        for triangle1 = (list (first triple1)
                              (first triple2)
                              (first triple3))
        for triangle2 = (list (second triple1)
                              (second triple2)
                              (second triple3))
        for triangle3 = (list (third triple1)
                              (third triple2)
                              (third triple3))
        collect triangle1
        collect triangle2
        collect triangle3))

(defun aoc2016/day3/solution2 ()
  (length (remove-if (lambda (triangle)
                       (not (is-triangle? triangle)))
                     (read-triangles-vertically))))
