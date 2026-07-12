(in-package #:advent-of-code)

(defun is-open-space-p (x y)
  ;; - Find x*x + 3*x + 2*x*y + y + y*y.
  ;; - Add the office designer's favorite number (your puzzle input).
  ;; - Find the binary representation of that sum; count the number of bits that are 1.
  ;;   - If the number of bits that are 1 is even, it's an open space.
  ;;   - If the number of bits that are 1 is odd, it's a wall
  (let* ((designer-favorite-number 1358)
         (magic-coords-number (+ (* x x)
                                 (* 3 x)
                                 (* 2 x y)
                                 y
                                 (* y y)
                                 designer-favorite-number))
         (binary-rep (format nil "~b" magic-coords-number)))
    (evenp (count #\1 binary-rep))))

(defun aoc2016/day13/solution1 ()
  (let ((office-map (make-array (list 100 100))))
    (multiple-value-bind (l distances)
        (general-shortest-path office-map
                               :start (cons 1 1)
                               :goal (cons 31 39)
                               :valid-p (lambda (dest src map)
                                          (declare (ignore src map))
                                          (is-open-space-p (car dest) (cdr dest))))
      (declare (ignore distances))
      l)))

(defun aoc2016/day13/solution2 ()
  (let ((office-map (make-array (list 100 100))))
    (multiple-value-bind (l distances)
        (general-shortest-path office-map
                               :start (cons 1 1)
                               :goal (cons 31 39)
                               :valid-p (lambda (dest src map)
                                          (declare (ignore src map))
                                          (is-open-space-p (car dest) (cdr dest))))
      (declare (ignore l))
      (let ((shape (array-dimensions distances)))
        (loop for r from 0 below (first shape)
              summing (loop for c from 0 below (second shape)
                        count (<= (aref distances r c) 50)))))))
