(in-package #:advent-of-code)

(defun read-beacons-and-sensors ()
  (loop for l in (uiop:read-file-lines
                  (asdf:system-relative-pathname :advent-of-code "inputs/2022/day15"))
        collect (register-groups-bind ((#'parse-integer sx sy bx by))
                    ("Sensor at x=([-\\d]+), y=([-\\d]+): closest beacon is at x=([-\\d]+), y=([-\\d]+)" l)
                  (list sx sy bx by))))

(defun distance (x1 y1 x2 y2)
  (+ (abs (- x1 x2))
     (abs (- y1 y2))))

(defun beacon-cannot-exist-p (x y beacons-and-sensors)
  (loop for (sx sy bx by) in beacons-and-sensors
          thereis (and
                   (not (and (= x bx)
                             (= y by)))
                   (<= (distance x y sx sy)
                       (distance sx sy bx by)))))

(defun beacon-can-exist-p (x y beacons-and-sensors)
  (loop for (sx sy bx by) in beacons-and-sensors
        always (> (distance x y sx sy)
                  (distance sx sy bx by))))

(defun circumference (cx cy r)
  (remove-duplicates
   (append (loop for x from (- cx r) to cx
                 for y from cy downto (- cy r)
                 collect (list x y))
           (loop for x from cx to (+ cx r)
                 for y from (- cy r) to cy
                 collect (list x y))
           (loop for x from (+ cx 1) downto cx
                 for y from cy to (+ cy r)
                 collect (list x y))
           (loop for x from cx downto (- cx r)
                 for y from (+ cy r) downto cy
                 collect (list x y)))
   :test 'equal))

(defun aoc2022/day15/solution1 ()
  (loop with beacons-and-sensors = (read-beacons-and-sensors)
        with y = 2000000                ; 10
        for x from -15000000 to 15000000
        count (beacon-cannot-exist-p x y beacons-and-sensors)))

(defun aoc2022/day15/solution2 ()
  (loop with beacons-and-sensors = (reverse (read-beacons-and-sensors))
        for (sx sy bx by) in beacons-and-sensors
        do (loop for (x y) in (circumference sx sy (+ 1 (distance sx sy bx by)))
                 when (and (<= 0 x 4000000)
                           (<= 0 y 4000000)
                           (beacon-can-exist-p x y beacons-and-sensors))
                   do                   ; (format t "~a,~a~%" x y)
                     (return-from aoc2022/day15/solution2 (+ (* 4000000 x) y)))))
