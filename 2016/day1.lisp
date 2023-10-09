(in-package #:advent-of-code)

(defun instructions ()
  (loop for instr in (split ", " (read-input-file "inputs/2016/day1"))
        collect
        (register-groups-bind (direction (#'parse-integer steps))
            ("(.)(\\d+)" instr)
          (list direction steps))))

(defun change-direction (heading turn)
  (case heading
    (:NORTH (if (string= turn "L")
                :WEST
                :EAST))
    (:WEST (if (string= turn "L")
                :SOUTH
                :NORTH))
    (:SOUTH (if (string= turn "L")
                :EAST
                :WEST))
    (:EAST (if (string= turn "L")
                :NORTH
                :SOUTH))))

(defun walk-and-collect-locations (x y heading instruction)
  (let* ((direction (first instruction))
         (steps (second instruction))
         (new-heading (change-direction heading direction)))
    (loop repeat steps
          do (case new-heading
               (:NORTH (incf y))
               (:SOUTH (decf y))
               (:EAST (incf x))
               (:WEST (decf x)))
          collect (list x y) into locations
          finally (return (list x y new-heading locations)))))

(defun walk-to-destination (&key with-visited-locations)
  (let ((heading :NORTH)
        (x 0)
        (y 0)
        (visited-locations nil))
    (dolist (i (instructions))
      (destructuring-bind (x1 y1 heading1 locs) (walk-and-collect-locations x y heading i)
        (setf x x1)
        (setf y y1)
        (setf heading heading1)
        (setf visited-locations (append visited-locations locs))))
    (if with-visited-locations
        (list x y visited-locations)
        (list x y))))

(defun aoc2016/day1/solution1 ()
  (destructuring-bind (x y) (walk-to-destination)
    (+ (abs x) (abs y))))

(defun aoc2016/day1/solution2 ()
  (destructuring-bind (x y visited-locations) (walk-to-destination :with-visited-locations t)
    (declare (ignore x y))
    (let ((visited-locations-h (make-hash-table :test 'equal)))
      (loop for l in visited-locations
            do (incf (gethash l visited-locations-h 0))
            when (= (gethash l visited-locations-h) 2)
              return (+ (abs (first l)) (abs (second l)))
            ))))
