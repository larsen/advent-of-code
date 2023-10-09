(in-package #:advent-of-code)

(defun read-code-procedure ()
  (read-input-file-as-lines "inputs/2016/day2"))

(defparameter *keypad1-mappings*
  '(
    ((#\1 #\L) . #\1) ((#\2 #\L) . #\1) ((#\3 #\L) . #\2)
    ((#\1 #\U) . #\1) ((#\2 #\U) . #\2) ((#\3 #\U) . #\3)
    ((#\1 #\D) . #\4) ((#\2 #\D) . #\5) ((#\3 #\D) . #\6)
    ((#\1 #\R) . #\2) ((#\2 #\R) . #\3) ((#\3 #\R) . #\3)

    ((#\4 #\L) . #\4) ((#\5 #\L) . #\4) ((#\6 #\L) . #\5)
    ((#\4 #\U) . #\1) ((#\5 #\U) . #\2) ((#\6 #\U) . #\3)
    ((#\4 #\D) . #\7) ((#\5 #\D) . #\8) ((#\6 #\D) . #\9)
    ((#\4 #\R) . #\5) ((#\5 #\R) . #\6) ((#\6 #\R) . #\6)

    ((#\7 #\L) . #\7) ((#\8 #\L) . #\7) ((#\9 #\L) . #\8)
    ((#\7 #\U) . #\4) ((#\8 #\U) . #\5) ((#\9 #\U) . #\6)
    ((#\7 #\D) . #\7) ((#\8 #\D) . #\8) ((#\9 #\D) . #\9)
    ((#\7 #\R) . #\8) ((#\8 #\R) . #\9) ((#\9 #\R) . #\9)))

(defparameter *keypad2-mappings*
  '(
    ((#\1 #\L) . #\1)
    ((#\1 #\U) . #\1)
    ((#\1 #\D) . #\3)
    ((#\1 #\R) . #\1)

    ((#\2 #\L) . #\2) ((#\3 #\L) . #\2) ((#\4 #\L) . #\3)
    ((#\2 #\U) . #\2) ((#\3 #\U) . #\1) ((#\4 #\U) . #\4)
    ((#\2 #\D) . #\6) ((#\3 #\D) . #\7) ((#\4 #\D) . #\8)
    ((#\2 #\R) . #\3) ((#\3 #\R) . #\4) ((#\4 #\R) . #\4)

    ((#\5 #\L) . #\5) ((#\6 #\L) . #\5) ((#\7 #\L) . #\6) ((#\8 #\L) . #\7) ((#\9 #\L) . #\8)
    ((#\5 #\U) . #\5) ((#\6 #\U) . #\2) ((#\7 #\U) . #\3) ((#\8 #\U) . #\4) ((#\9 #\U) . #\9)
    ((#\5 #\D) . #\5) ((#\6 #\D) . #\A) ((#\7 #\D) . #\B) ((#\8 #\D) . #\C) ((#\9 #\D) . #\9)
    ((#\5 #\R) . #\6) ((#\6 #\R) . #\7) ((#\7 #\R) . #\8) ((#\8 #\R) . #\9) ((#\9 #\R) . #\9)

    ((#\A #\L) . #\A) ((#\B #\L) . #\A) ((#\C #\L) . #\B)
    ((#\A #\U) . #\6) ((#\B #\U) . #\7) ((#\C #\U) . #\8)
    ((#\A #\D) . #\A) ((#\B #\D) . #\D) ((#\C #\D) . #\C)
    ((#\A #\R) . #\B) ((#\B #\R) . #\C) ((#\C #\R) . #\C)

    ((#\D #\L) . #\D)
    ((#\D #\U) . #\B)
    ((#\D #\D) . #\D)
    ((#\D #\R) . #\D)))

(defun reach-new-button (starting-button direction keypad-mappings)
  (cdr (assoc (list starting-button direction) keypad-mappings :test 'equal)))

(defun apply-procedure-line (starting-button procedure-line
                             &optional (keypad-mappings *keypad1-mappings*))
  (reduce (lambda (b dir)
            (reach-new-button b dir keypad-mappings))
          procedure-line :initial-value starting-button))

(defun aoc2016/day2/solution1 ()
  (let ((initial-button #\1)
        (code-sequence '()))
    (dolist (l (read-code-procedure))
      (let ((new-button (apply-procedure-line initial-button l)))
        (setf code-sequence (nconc code-sequence (list new-button)))
        (setf initial-button new-button)))
    (coerce code-sequence 'string)))

(defun aoc2016/day2/solution2 ()
  (let ((initial-button #\5)
        (code-sequence '()))
    (dolist (l (read-code-procedure))
      (let ((new-button (apply-procedure-line initial-button l *keypad2-mappings*)))
        (setf code-sequence (nconc code-sequence (list new-button)))
        (setf initial-button new-button)))
    (coerce code-sequence 'string)))
