(in-package #:advent2016)

(defclass bot ()
  ((input-bin :accessor input)
   (lower-value :accessor lower-value)
   (higher-value :accessor higher-value)))

(defun read-bots-instructions ()
  (uiop:read-file-lines
   (asdf:system-relative-pathname 'advent2016 "inputs/day10")))

(defun day10/solution1 ()
  (let ((bots (make-hash-table)))
    (flet ((get-bot-n (n)
             (gethash n bots (make-instance 'bot))))
      (loop for instr in (read-bots-instructions)
            do (register-groups-bind ((#'parse-integer value) (#'parse-integer bot))
                   ("value (\\d+) goes to bot (\\d+)" instr)
                 (setf (input (get-bot-n bot)) value))
               (register-groups-bind ((#'parse-integer bot)
                                      (#'parse-integer low-value-recipient)
                                      (#'parse-integer high-value-recipient))
                   ("bot (\\d+) gives low to bot (\\d) and high to bot (\\d+)" instr)
                 (setf (input (get-bot-n low-value-recipient)) (lower-value bot))
                 (setf (input (get-bot-n high-value-recipient)) (higher-value bot)))))))

(defun day10/solution2 ()
  )
