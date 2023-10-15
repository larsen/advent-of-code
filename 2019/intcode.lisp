(in-package #:advent-of-code)

(defclass cpu ()
  ((memory :initarg :memory :accessor mem)
   (ip :initarg :ip :initform 0 :accessor ip)
   (relative-base :initform 0 :accessor relative-base)
   (halt :initform nil :accessor halt)
   (input :initarg :input :initform '() :accessor cpu-input)
   (output :initform '() :accessor output)))

(defmethod print-object ((cpu cpu) stream)
  (with-slots (halt ip relative-base input output) cpu
    (format stream "H: ~A IP: ~A RB: ~A In: ~A Out: ~A~%"
            halt ip relative-base input output)))

(defmethod reset! ((cpu cpu))
  (setf (ip cpu) 0)
  (setf (halt cpu) nil))

(defmethod incip! ((cpu cpu))
  (incf (ip cpu)))

(defgeneric peek (cpu &optional location))
(defmethod peek ((cpu cpu) &optional location)
  "Returns the value in CPU memory at LOCATION (absolute).
If LOCATION is not specified, it uses the Instruction pointer"
  (handler-case (aref (mem cpu) (or location (ip cpu)))
    (sb-int:invalid-array-index-error ()
      (error "peek - it didn't go well!"))))

(defgeneric poke (cpu location value))
(defmethod poke ((cpu cpu) location value)
  (handler-case (setf (aref (mem cpu) location)
                      value)
    (sb-int:invalid-array-index-error ()
      ;; enlarge memory, reload the program and try again
      (let ((mc (mem cpu)))
        (setf mc (mem cpu))
        (setf (mem cpu) (concatenate 'vector mc (make-array (length mc) :adjustable t)))
        (poke cpu location value)))))

;; (defmethod disassemble-memory ((cpu cpu) &optional from)
;;   (loop with i = (or from (ip cpu))
;;         do (setf i (intcode-disassemble (current-opcode cpu i) cpu))
;;         until (>= i (length (mem cpu)))))

(defmethod consume-input! ((cpu cpu))
  (let ((result (car (cpu-input cpu))))
    (setf (cpu-input cpu) (rest (cpu-input cpu)))
    result))

(defmethod emit-output! ((cpu cpu))
  (let ((result (first (last (output cpu)))))
    (set (output cpu)
         (but-last (output cpu)))))

(defmethod opcode-at (location (cpu cpu))
  (let ((canonical-opcode-lst (pad (digits (peek cpu location)) 5 0)))
    (+ (* 10 (nth 3 canonical-opcode-lst))
       (nth 4 canonical-opcode-lst))))

(defgeneric current-opcode (cpu &optional at))
(defmethod current-opcode ((cpu cpu) &optional at)
  (opcode-at (or at (ip cpu)) cpu))

(defun read-raw-program-sequence (filename)
  (mapcar #'parse-integer (split-sequence #\, (uiop:read-file-string filename))))

(defun intcode-read-program (filename)
  (let ((source (read-raw-program-sequence filename)))
    (make-array (length source) :adjustable t
                                :initial-contents source)))

(defmethod load-program ((cpu cpu) program)
  (setf (mem cpu) program))

(defmethod read-program ((cpu cpu) filename)
  (let ((source (read-raw-program-sequence filename)))
    (load-program cpu (intcode-read-program filename))))

(defun opcode-details (opcode)
  (labels ((code->param-mode (code)
             (case code
               (0 :position-mode)
               (1 :immediate-mode)
               (2 :relative-mode))))
    (let ((canonical-opcode-lst (pad (digits opcode) 5 0)))
      (list (code->param-mode (nth 2 canonical-opcode-lst))
            (code->param-mode (nth 1 canonical-opcode-lst))
            (code->param-mode (nth 0 canonical-opcode-lst))
            (+ (* 10 (nth 3 canonical-opcode-lst))
               (nth 4 canonical-opcode-lst))))))

(defmethod get-param ((cpu cpu) param param-mode)
  (cond ((eql :position-mode param-mode) (peek cpu param))
        ((eql :relative-mode param-mode) (peek cpu (+ param (relative-base cpu))))
        ((eql :immediate-mode param-mode) param)
        (t (error (format nil "Unknown mode ~a!" param-mode)) )))

(defmacro definstruction (name opcode params &body body)
  (declare (ignorable name))
  `(progn
     ;; (defmethod intcode-disassemble ((opcode (eql ,opcode)) (cpu cpu)
     ;;                         &optional (stream *standard-output*))
     ;;   (destructuring-bind (&rest param-modes)
     ;;       (opcode-details (peek cpu))
     ;;     (format stream "~&~A~%" (symbol-name ,name)))
     ;;   ,(+ 1 (length (remove '&out params))))
     (defmethod execute ((opcode (eql ,opcode)) (cpu cpu))
       (destructuring-bind (&rest param-modes)
           (opcode-details (peek cpu))
         (incip! cpu)
         (let (,@(loop for p in (remove '&out params)
                       collect `(,p nil)))
           ,@(when params
               (let* ((partitioned-params (split-sequence:split-sequence '&out params))
                      (param-counter -1)
                      (in-params (first partitioned-params))
                      (out-params (second partitioned-params)))
                 (append (loop
                           for p in in-params
                           ;; First we get what's in memory at IP
                           collect `(setf ,p (peek cpu))
                           collect '(incip! cpu)
                           ;; Then we decide if that's a value (immediate mode)
                           ;; or a position (position mode)
                           ;; or a relative position (relative mode)
                           collect `(setf ,p (get-param cpu ,p
                                                        (nth ,(incf param-counter)
                                                             param-modes))))
                         (loop
                           for p in out-params
                           collect `(setf ,p (peek cpu))
                           collect '(incip! cpu)
                           collect `(when (eql :relative-mode
                                               (nth ,(incf param-counter) param-modes))
                                      (incf ,p (relative-base cpu))))
                         )))
           ,@body)))))

(definstruction :halt 99 ()
  (setf (halt cpu) t))

(definstruction :addition 1 (op1 op2 &out dest)
  (poke cpu dest (+ op1 op2)))

(definstruction :multiplication 2 (op1 op2 &out dest)
  (poke cpu dest (* op1 op2)))

(definstruction :input 3 (&out dest)
  (let ((in (consume-input! cpu)))
    (poke cpu dest in)))

(definstruction :output 4 (op1)
  (push op1 (output cpu)))

(definstruction :jump-if-true 5 (op1 op2)
  (when (not (zerop op1))
    (setf (ip cpu) op2)))

(definstruction :jump-if-false 6 (op1 op2)
  (when (zerop op1)
    (setf (ip cpu) op2)))

(definstruction :less-than 7 (op1 op2 &out dest)
  (setf (aref (mem cpu) dest)
        (if (< op1 op2) 1 0)))

(definstruction :equals 8 (op1 op2 &out dest)
  (setf (aref (mem cpu) dest)
        (if (= op1 op2) 1 0)))

(definstruction :adjust-relative-base 9 (op1)
  (incf (relative-base cpu) op1))

(defmethod run! ((cpu cpu))
  "Runs the CPU until it reaches the halting state."
  (loop while (not (halt cpu))
        do (execute (current-opcode cpu) cpu)))

(defgeneric get-output! (cpu &optional input))
(defmethod get-output! ((cpu cpu) &optional input)
  "Runs the CPU until there is some value in output.
Optionally, it enqueues INPUT in the CPU's input list."
  (when input
    (setf (cpu-input)
          (append (last (cpu-input cpu))
                  input)))
  (loop while (not (output cpu))
        do (execute (current-opcode cpu) cpu)
        finally (return (emit-output! cpu))))

(defun run-and-get-output! (program input)
  "Runs the CPU until it reaches the halting state, then
returns the last value output."
  (let ((cpu (make-instance 'cpu :input input)))
    (setf (mem cpu) program)
    (run! cpu)
    (first (output cpu))))
