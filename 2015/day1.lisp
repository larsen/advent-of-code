(in-package #:advent-of-code)

(defun read-floor-instructions ()
  (uiop:read-file-string (asdf:system-relative-pathname 'advent-of-code
                                                        "inputs/2015/day1")))

(defun aoc2015/day1/solution1 ()
  (let ((floor 0))
    (loop for c across (read-floor-instructions)
          if (char-equal c #\()
            do (incf floor)
          if (char-equal c #\))
            do (decf floor)
          finally (return floor))))

(defun aoc2015/day1/solution2 ()
  (let ((floor 0)
        (pos 1))
    (loop for c across (read-floor-instructions)
          if (char-equal c #\()
            do (incf floor)
               (incf pos)
          if (char-equal c #\))
            do (decf floor)
               (incf pos)
          until (= floor -1)
          finally (return (- pos 1)))))
