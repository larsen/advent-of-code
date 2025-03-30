(in-package #:advent-of-code)

(defun read-monitoring-station-asteroids ()
  (loop for line in (uiop:read-file-lines
                     (asdf:system-relative-pathname
                      'advent-of-code "inputs/2019/day10"))
        for y from 0
        append (loop for c across line
                     for x from 0
                     when (char= c #\#)
                       collect (v2! x y))))

(defun unique-elements (lst cmp-fn)
  "Return a list containing the unique elements of LST based on CMP-FN."
  (let ((result '())
        (seen '()))
    (dolist (element lst (nreverse result))
      (unless (member element seen :test cmp-fn)
        (push element seen)
        (push element result)))))


(defun slope-and-direction (a1 a2)
  (v2:normalize (v2:- a1 a2)))

(defun same-slope-p (s1 s2)
  (< (abs (- (v2:angle-between s1 s2) 0))
     0.001))

(defun count-visible-from (candidate asteroids)
  (length (unique-elements (loop for a in asteroids
                                 when (not (equalp a candidate))
                                   collect (slope-and-direction candidate a))
                           'same-slope-p)))

(defun aoc2019/day10/solution1 ()
  (let ((asteroids (read-monitoring-station-asteroids)))
    (loop with solution = nil
          with max-asteroids-visible = 0
          for candidate in asteroids
          for visible-count = (count-visible-from candidate asteroids)
          when (> visible-count max-asteroids-visible)
            do (setf solution candidate)
               (setf max-asteroids-visible visible-count)
          finally (return max-asteroids-visible))))

(defun aoc2019/day10/solution2 ()
  )
