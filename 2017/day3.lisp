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

;; The second part seems to require some radically different
;; technique.  For a given square on the spiral, are we abel to
;; compute the values stored in the 8 Manhattan directions?
;; If they are treated as indexes in an array of solutions, we
;; can compute the value for a square recursively
;;
;; f(n) = sum( f(n') for n' being the index in all the directions,
;;             except those that result in indexes larger than n )
;;
;; Let's try first to build a correspondence between
;; the natural numbers and coordinates (we'll need to use the reverse
;; of this, later)

(defun n-to-coordinates (n)
  (loop with initial-x = 0
        with initial-y = 0
        with dirs = #(up left down right)
        for counter from 0
        repeat n
        collect (aref dirs (mod counter 4))))

(defun aoc2017/day3/solution2 ()
  )
