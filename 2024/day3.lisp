(in-package #:advent-of-code)

(defun read-corrupt-memory ()
  (read-input-file "inputs/2024/day3"))

(defun parse-and-exec-mul-instruction (instruction)
  (cl-ppcre:register-groups-bind ((#'parse-integer x y))
       ("mul\\((\\d{1,3}),(\\d{1,3})\\)" instruction)
    (* x y)))

(defun aoc2024/day3/solution1 ()
  (loop for match in (all-matches-as-strings
                      "mul\\(\\d{1,3},\\d{1,3}\\)" (read-corrupt-memory))
        sum (parse-and-exec-mul-instruction match)))

(defun join-regexps (strings)
  (format nil "~{(~A)~^|~}" strings))

(defun aoc2024/day3/solution2 ()
  (let* ((instruction-regexps '("mul\\(\\d{1,3},\\d{1,3}\\)"
                                "do\\(\\)"
                                "don't\\(\\)"))
         (regexp (join-regexps instruction-regexps))
         (instructions (all-matches-as-strings regexp (read-corrupt-memory))))
    (loop with mul-allowed = T
          with acc = 0
          for instr in instructions
          do (cond
               ((string= "do()" instr) (setf mul-allowed T))
               ((string= "don't()" instr) (setf mul-allowed nil))
               (T (when mul-allowed
                    (incf acc (parse-and-exec-mul-instruction instr)))))
          finally (return acc))))
