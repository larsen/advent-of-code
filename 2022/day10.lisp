(in-package #:advent-of-code)

(defun read-program ()
  (mapcar (lambda (l)
            (split " " l))
          (uiop:read-file-lines
           (asdf:system-relative-pathname :advent-of-code "inputs/2022/day10"))))

(defstruct elves-device
  (program)
  (cycle 0)
  (reg-x 1)
  (samplings '())
  (display (make-array '(6 40) :initial-element #\.)))

(defmethod light-up-current-pixel ((d elves-device))
  (with-slots (cycle display) d
    (setf (aref display
                (floor (- cycle 1) 40)
                (rem (- cycle 1) 40))
          #\#)))

(defmethod tick ((d elves-device))
  (with-slots (cycle reg-x samplings) d
    (incf cycle)

    ; (format t "~a ~a ~a ~%" cycle (- (rem cycle 40) 1) reg-x)

    (if (<= (- reg-x 1)
            (rem (- cycle 1) 40)
            (+ reg-x 1))
        (light-up-current-pixel d))

    (if (member cycle '(20 60 100 140 180 220))
        (push (cons cycle reg-x)
              samplings))))

(defmethod render-display ((d elves-device))
  (loop for r from 0 below 6
        do (loop for c from 0 below 40
                 do (format t "~c" (aref (elves-device-display d) r c)))
           (format t "~%")))

(defmethod print-object ((d elves-device) stream)
  (format stream "CYCLE: ~a   REG X: ~a~%"
          (elves-device-cycle d)
          (elves-device-reg-x d))
  (render-display d))

(defmethod run-program-on-elves-device ((d elves-device))
  (loop for (opcode param) in (elves-device-program d)

        when (string= "noop" opcode)
          do (tick d)

        when (string= "addx" opcode)
          do (tick d)
             (tick d)
             (incf (elves-device-reg-x d)
                   (parse-integer param))))

(defun aoc2022/day10/solution1 ()
  (let ((device (make-elves-device :program (read-program))))
    (run-program-on-elves-device device)
    (sum (loop for (c . r) in (elves-device-samplings device)
               collect (* c r)))))

(defun aoc2022/day10/solution2 ()
  (let ((device (make-elves-device :program (read-program))))
    (run-program-on-elves-device device)
    ;; (render-display device)
    "RGZEHURK"))
