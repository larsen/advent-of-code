(in-package #:advent-of-code)

(defun read-raw-program-sequence (filename)
  (mapcar #'parse-integer
          (split-sequence #\, (uiop:read-file-string filename))))

(defun read-program (filename)
  (let ((source (read-raw-program-sequence filename)))
    (make-array (length source) :adjustable t
                                :initial-contents source)))

(defun run-program (prg)
  ;; Instruction pointer
  (let ((ip 0))
    (loop for opcode = (aref prg ip)
          do (case opcode
               ;; Addition
               (1 (let ((op1 (aref prg (+ ip 1)))
                          (op2 (aref prg (+ ip 2)))
                          (dest (aref prg (+ ip 3))))
                      (setf (aref prg dest)
                            (+ (aref prg op1)
                               (aref prg op2)))
                      (incf ip 4)))
               ;; Multiplication
               (2 (let ((op1 (aref prg (+ ip 1)))
                          (op2 (aref prg (+ ip 2)))
                          (dest (aref prg (+ ip 3))))
                      (setf (aref prg dest)
                            (* (aref prg op1)
                               (aref prg op2)))
                    (incf ip 4))))
          ;; Halt
          until (= opcode 99))
    prg))
