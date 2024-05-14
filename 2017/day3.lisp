(in-package #:advent-of-code)

(defparameter *2017/day3/input* 347991)

;; 17  16  15  14  13
;; 18   5   4   3  12
;; 19   6   1   2  11
;; 20   7   8   9  10
;; 21  22  23---> ...

(defun spiral-diameter (n)
  "Given a number N, returns the diameter of the numeric
'circumference' where it lays."
  (loop for d from 1 by 2
        until (<= n (* d d))
        finally (return d)))

(defun middle-points-squared-spiral (d)
  "Given the diameter D of a numerical 'circonference',
returns a list of the middle points of it. In the example above, for
diameter D = 5, these numbers would be '(23 19 15 11)"
  (loop repeat 4
        with radius = (floor (/ d 2))
        for p = (- (* d d) radius)
          then (- p (- d 1))
        collect p))

(defun min-distance-from-a-list (n lst)
  "Given a NUMBER a list LST of numbers, returns the distance from the
closest"
  (loop for k in lst
        minimizing (abs (- k n))))

(defun spiral-manhattan-distance (n)
  (let* ((diameter (spiral-diameter n))
         (radius (floor (/ diameter 2))))
    (+ radius
       (min-distance-from-a-list n (middle-points-squared-spiral diameter)))))

(defun aoc2017/day3/solution1 ()
  (spiral-manhattan-distance *2017/day3/input*))


(defparameter *adjacent-cells-directions*
  (list #c(-1 -1) #c(0 -1) #c(1 -1)
        #c(-1 0)      #c(1 0)
        #c(-1 1) #c(0 1) #c(1 1)))

(defun aoc2017/day3/solution2 ()
  (let* ((grid (make-hash-table))
         (directions `((:right . ,#c(1 0))
                       (:up    . ,#c(0 -1))
                       (:left  . ,#c(-1 0))
                       (:down  . ,#c(0 1))))
         (directions-iterator (make-instance 'wrapped-sequence
                                             :sequence (mapcar #'car directions)))
         (pos #c(0 0))
         last-value-found)
    (setf (gethash pos grid) 1)
    (setf last-value-found 1)
    ;; This is a spiraling "stepper"
    (loop for n from 1
          do (loop repeat 2
                   for dir = (next directions-iterator)
                   do (loop repeat n
                            do (incf pos (cdr (assoc dir directions)))
                               (setf last-value-found
                                     (safesum (mapcar
                                               (lambda (d) (gethash (+ pos d) grid nil))
                                               *adjacent-cells-directions*)))
                               (setf (gethash pos grid) last-value-found)
                            when (> last-value-found *2017/day3/input*)
                              do (return-from aoc2017/day3/solution2 last-value-found))))))
