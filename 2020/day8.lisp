(in-package #:advent-of-code)

(defun aoc2020/day8/read-program ()
  (let ((source-code
          (mapcar #'opcode-and-param (uiop:read-file-lines
                                      (asdf:system-relative-pathname
                                       'advent-of-code "inputs/2020/day8")))))
    (make-array (length source-code) :initial-contents source-code)))

(defun opcode-and-param (line-of-code)
  (destructuring-bind (opcode param)
      (split-sequence #\Space line-of-code)
    (cons (make-keyword (string-upcase opcode))
          (parse-integer param))))

(defun aoc2020/day8/run-program (program)
  (let ((accumulator 0)
        (pc 0)
        (execution-counters (make-hash-table :test #'equal)))
    (loop while (< pc (length program))
          for (opcode . param) = (aref program pc)
          do (incf (gethash pc execution-counters 0))
             (if (= 2 (gethash pc execution-counters))
                 (return-from aoc2020/day8/run-program (cons pc accumulator)))
             (case opcode
               (:nop
                (incf pc 1))
               (:acc
                (incf accumulator param)
                (incf pc 1))
               (:jmp
                (incf pc param))
               (otherwise (error "Unknown instruction!")))
          finally (return (cons pc accumulator)))))

(defun aoc2020/day8/solution1 ()
  (cdr (aoc2020/day8/run-program (aoc2020/day8/read-program))))

(defun copy-program (program)
  (make-array
   (length program)
   :initial-contents (loop for (opcode . param) across program
                           collect (cons opcode param))))

(defun aoc2020/day8/solution2 ()
  (labels ((flip (opcode)
             (ecase opcode
               (:nop :jmp)
               (:jmp :nop))))
    (let ((original-program (aoc2020/day8/read-program))
          program)
      (symbol-macrolet ((opcode (car (aref program instruction-to-change))))
        (loop for instruction-to-change below (length original-program)
              do (setf program (copy-program original-program))
              when (member opcode '(:nop :jmp))
                do (setf opcode (flip opcode))
                   (let ((pc-and-acc (aoc2020/day8/run-program program)))
                     (if (= (car pc-and-acc)
                            (length program))
                         (return-from aoc2020/day8/solution2 (cdr pc-and-acc)))))))))
