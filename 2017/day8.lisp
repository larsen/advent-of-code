(in-package #:advent-of-code)

(defun read-register-instructions ()
  (loop for line in (read-input-file-as-lines "inputs/2017/day8")
        collect (register-groups-bind (register
                                       op
                                       (#'parse-integer val)
                                       comp-reg
                                       comparator
                                       (#'parse-integer comp-val))
                    ("(\\w+) (\\w+) ([0-9-]+) if (\\w+) (.+) ([0-9-]+)$" line)
                  (list register op val comp-reg comparator comp-val))))

(defun evaluate-comparison (register comparator value registers)
  (let ((register-value (gethash register registers 0)))
    (cond
      ((string= comparator ">") (> register-value value))
      ((string= comparator "<") (< register-value value))
      ((string= comparator "<=") (<= register-value value))
      ((string= comparator ">=") (>= register-value value))
      ((string= comparator "==") (= register-value value))
      ((string= comparator "!=") (not (= register-value value))))))

(defun aoc2017/day8/solution1 ()
  (let ((registers (make-hash-table :test 'equalp))
        (instructions (read-register-instructions)))
    (loop for (reg op val comp-reg comp comp-val) in instructions
          do (if (evaluate-comparison comp-reg comp comp-val registers)
                 (if (string= op "inc")
                     (incf (gethash reg registers 0) val)
                     (decf (gethash reg registers 0) val))))
    (loop for v being the hash-values of registers
          maximizing v)))

(defun aoc2017/day8/solution2 ()
  (let ((registers (make-hash-table :test 'equalp))
        (register-highest-values (make-hash-table :test 'equalp))
        (instructions (read-register-instructions)))
    (loop for (reg op val comp-reg comp comp-val) in instructions
          do (if (evaluate-comparison comp-reg comp comp-val registers)
                 (if (string= op "inc")
                     (incf (gethash reg registers 0) val)
                     (decf (gethash reg registers 0) val)))
             (loop for reg being the hash-keys of registers
                   when (> (gethash reg registers)
                           (gethash reg register-highest-values 0))
                     do (setf (gethash reg register-highest-values)
                              (gethash reg registers))))
    (loop for v being the hash-values of register-highest-values
          maximizing v)))
