(in-package #:advent-of-code)

(defclass billboard ()
  ((lights :initform (make-array '(1000 1000)
                                 :initial-element nil
                                 :element-type 'boolean )
           :accessor lights)))

(defclass billboard-dimmerable ()
  ((lights :initform (make-array '(1000 1000))
           :accessor lights)))

(defgeneric flip (b x y))
(defmethod flip ((billboard billboard) x y)
  (setf (aref (lights billboard) x y)
        (not (aref (lights billboard) x y))))

(defgeneric flip-rectangle (b x1 y1 x2 y2))
(defmethod flip-rectangle ((billboard billboard) x1 y1 x2 y2)
  (loop for x from x1 to x2
        do (loop for y from y1 to y2
                 do (flip billboard x y))))

(defgeneric turn-on-rectangle (b x1 y1 x2 y2))
(defmethod turn-on-rectangle ((billboard billboard) x1 y1 x2 y2)
  (loop for x from x1 to x2
        do (loop for y from y1 to y2
                 do (setf (aref (lights billboard) x y) t))))

(defgeneric turn-off-rectangle (b x1 y1 x2 y2))
(defmethod turn-off-rectangle ((billboard billboard) x1 y1 x2 y2)
  (loop for x from x1 to x2
        do (loop for y from y1 to y2
                 do (setf (aref (lights billboard) x y) nil))))

(defgeneric count-lights-on (billboard))
(defmethod count-lights-on ((billboard billboard))
  (let ((counter 0))
    (loop for x from 0 to 999
          do (loop for y from 0 to 999
                   do (if (aref (lights billboard) x y)
                          (incf counter)))
          finally (return counter))))



(defgeneric increase-brightness (b x1 y1 x2 y2))
(defmethod increase-brightness ((billboard billboard-dimmerable)
                                x1 y1 x2 y2)
  (loop for x from x1 to x2
        do (loop for y from y1 to y2
                 do (incf (aref (lights billboard) x y)))))

(defgeneric decrease-brightness (b x1 y1 x2 y2))
(defmethod decrease-brightness ((billboard billboard-dimmerable)
                                x1 y1 x2 y2)
  (loop for x from x1 to x2
        do (loop for y from y1 to y2
                 do (decf (aref (lights billboard) x y))
                    (if (< (aref (lights billboard) x y) 0)
                        (setf (aref (lights billboard) x y) 0)))))

(defgeneric toggle-brightness (b x1 y1 x2 y2))
(defmethod toggle-brightness ((billboard billboard-dimmerable)
                              x1 y1 x2 y2)
  (loop for x from x1 to x2
        do (loop for y from y1 to y2
                 do (incf (aref (lights billboard) x y) 2))))

(defgeneric count-brightness (billboard))
(defmethod count-brightness ((billboard billboard-dimmerable))
  (let ((counter 0))
    (loop for x from 0 to 999
          do (loop for y from 0 to 999
                   do (incf counter (aref (lights billboard) x y)))
          finally (return counter))))



(defun parse-command (line)
  (register-groups-bind (command
                         (#'parse-integer x1)
                         (#'parse-integer y1)
                         (#'parse-integer x2)
                         (#'parse-integer y2))
      ("(turn on|turn off|toggle) (\\d+),(\\d+) through (\\d+),(\\d+)" line)
    (list (switch (command :test #'string=)
            ("turn on" :turn-on)
            ("turn off" :turn-off)
            ("toggle" :toggle))
          x1 y1 x2 y2)))

(defun read-light-instructions ()
  (loop for l in (uiop:read-file-lines
                  (asdf:system-relative-pathname 'advent-of-code
                                                 "inputs/2015/day6"))
        collect (parse-command l)))

(defun aoc2015/day6/solution1 ()
  (let ((bb (make-instance 'billboard)))
    (loop for instruction in (read-light-instructions)
          do (destructuring-bind (command x1 y1 x2 y2)
                 instruction
               (case command
                 ((:turn-on) (turn-on-rectangle bb x1 y1 x2 y2))
                 ((:turn-off) (turn-off-rectangle bb x1 y1 x2 y2))
                 ((:toggle) (flip-rectangle bb x1 y1 x2 y2))))
          finally (return (count-lights-on bb)))))

(defun aoc2015/day6/solution2 ()
  (let ((bb (make-instance 'billboard-dimmerable)))
    (loop for instruction in (read-light-instructions)
          do (destructuring-bind (command x1 y1 x2 y2)
                 instruction
               (case command
                 ((:turn-on) (increase-brightness bb x1 y1 x2 y2))
                 ((:turn-off) (decrease-brightness bb x1 y1 x2 y2))
                 ((:toggle) (toggle-brightness bb x1 y1 x2 y2))))
          finally (return (count-brightness bb)))))
