(in-package #:advent-of-code)

(defun read-universe-galaxies ()
  (let* ((content (read-input-file-as-lines "inputs/2023/day11"))
         (width (length (first content)))
         (height (length content))
         (universe (make-array (list height width) :initial-contents content)))
    universe))

(defun print-universe (universe)
  (let* ((universe-dims (array-dimensions universe))
         (universe-h (first universe-dims))
         (universe-w (second universe-dims)))
    (loop for j from 0 below universe-h
          do (loop for i from 0 below universe-w
                   do (format t "~a" (aref universe j i)))
             (format t "~%"))))

(defun stars (universe)
  (let* ((universe-dims (array-dimensions universe))
         (universe-h (first universe-dims))
         (universe-w (second universe-dims)))
    (loop for x from 0 below universe-w
          append (loop for y from 0 below universe-h
                       when (char= #\# (aref universe y x))
                         collect (cons y x)))))

(defun expand-universe (universe &key (expand-factor 1))
  (let* ((universe-dims (array-dimensions universe))
         (universe-h (first universe-dims))
         (universe-w (second universe-dims))
         (expand-factor (clamp (- expand-factor 1)
                               1
                               most-positive-fixnum))
         (empty-cols (loop for i from 0 below universe-w
                           when (loop for j from 0 below universe-h
                                      always (char= #\. (aref universe j i)))
                             collect i))
         (empty-rows (loop for j from 0 below universe-h
                           when (loop for i from 0 below universe-w
                                      always (char= #\. (aref universe j i)))
                             collect j))
         (stars (stars universe)))
    ;; Now we have to relocate stars in the empty universe,
    ;; taking into account the expansion of empty cols and rows
    (loop for (y . x) in stars
          collect (cons (+ y (* expand-factor (count-if
                                               (lambda (v)
                                                 (< v y))
                                               empty-rows)))
                        (+ x (* expand-factor (count-if
                                               (lambda (v)
                                                 (< v x))
                                               empty-cols)))))))

(defun aoc2023/day11/solution1 ()
  (let* ((stars (expand-universe (read-universe-galaxies))))
    (/
     (loop for s1 in stars
           sum (loop for s2 in (remove s1 stars)
                     sum (+ (abs (- (car s1) (car s2)))
                            (abs (- (cdr s1) (cdr s2))))))
     2)))

(defun aoc2023/day11/solution2 ()
  (let* ((stars (expand-universe (read-universe-galaxies)
                                 :expand-factor 1000000)))
    (/
     (loop for s1 in stars
           sum (loop for s2 in (remove s1 stars)
                     sum (+ (abs (- (car s1) (car s2)))
                            (abs (- (cdr s1) (cdr s2))))))
     2)))
