(in-package #:advent-of-code)

(defun read-orbits ()
  (loop with orbital-map = (make-hash-table :test 'equal)
        for line in (uiop:read-file-lines (asdf:system-relative-pathname
                                           'advent-of-code "inputs/2019/day6"))
        do (destructuring-bind (planet orbiter-planet)
               (split-sequence #\) line)
             (setf (gethash orbiter-planet orbital-map) planet))
        finally (return orbital-map)))

(defun count-all-orbits (orbiter-planet orbital-map)
  (loop for ob = orbiter-planet
          then (gethash ob orbital-map)
        while ob
        count (not (null (gethash ob orbital-map)))))

(defun aoc2019/day6/solution1 ()
  (loop with orbital-map = (read-orbits)
        for p being the hash-keys of orbital-map
        sum (count-all-orbits p orbital-map)))

(defun extended-orbit (orbiter-planet orbital-map)
  (loop for ob = orbiter-planet
          then (gethash ob orbital-map)
        while ob
        collect ob))

(defun aoc2019/day6/solution2 ()
  (let* ((orbital-map (read-orbits))
         (extended-orbit-you (extended-orbit "YOU" orbital-map))
         (extended-orbit-san (extended-orbit "SAN" orbital-map))
         (first-common-orbit (first (last (intersection extended-orbit-you
                                                        extended-orbit-san
                                                        :test 'string=)))))
    (- (+ (find-index first-common-orbit extended-orbit-you :test 'string=)
          (find-index first-common-orbit extended-orbit-san :test 'string=))
       ;; -2 to account for the fact we include the starting planets
       2)))
