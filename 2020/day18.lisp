(in-package #:advent-of-code)

(defun read-expressions ()
  (uiop:read-file-lines
   (asdf:system-relative-pathname 'advent-of-code "inputs/2020/day18")))

(defun parse (expr)
  (split "\\s+|(|)" expr))

;; As a memento, here the code I wrote for the first part
;; Not malleable enough for the second part.
(defun funky-eval (tokens)
  (let ((stack nil))
    (loop while (not (null tokens))
          do (let ((token (pop tokens)))
               (if (scan "\\)" token)
                   (return-from funky-eval (values (pop stack)
                                                   tokens)))
               (if (scan "\\(" token)
                   (multiple-value-bind (res new-tokens)
                       (funky-eval tokens)
                     (push res stack)
                     (setf tokens new-tokens)))
               (if (scan "\\d+" token)
                   (push (parse-integer token) stack))
               (if (scan "\\+" token)
                   (push (+ (pop stack)
                            (if (scan "\\(" (car tokens))
                                (multiple-value-bind (res new-tokens)
                                    (funky-eval (cdr tokens))
                                  (setf tokens new-tokens)
                                  res)
                                (parse-integer (pop tokens))))
                         stack))
               (if (scan "\\*" token)
                   (push (* (pop stack)
                            (if (scan "\\(" (car tokens))
                                (multiple-value-bind (res new-tokens)
                                    (funky-eval (cdr tokens))
                                  (setf tokens new-tokens)
                                  res)
                                (parse-integer (pop tokens))))
                         stack)))
          finally (return (pop stack)))))

(defun aoc2020/day18/solution1 ()
  (apply #'+ (loop for expr in (read-expressions)
                   collect (funky-eval (parse expr)))))

(defun compute (rpn-expr)
  (loop with stack = nil
        for token in rpn-expr
        do (format t "considering ~a ~a ~a ~%" token (numberp token) (functionp token))
        when (numberp token)
          do (push token stack)
        when (functionp token)
          do (push (funcall token (pop stack) (pop stack))
                   stack)
        finally (return stack)))

(defun expr->rpn (expr)
  (loop for term in expr
        with stack = nil
        with op = nil
        do (cond ((scan "\\d" term)
                  (push (parse-integer term) stack))
                 ((string= "*" term)
                  (when (functionp (first op))
                    (push (pop op) stack))
                  (push #'* op))
                 ((string= "+" term)
                  (when (functionp (first op))
                    (push (pop op) stack))
                  (push #'+ op))
                 ((string= "(" term)
                  (loop for o = (pop op)
                        until (equal "(" o)
                        do (push o stack))))
        finally (loop for o in op
                      do (push o stack))
                (return (reverse stack))))

(defun aoc2020/day18/solution2 ()
    (apply #'+ (loop for expr in (read-expressions)
                     collect
                     (eval (read-from-string (concatenate 'string "#I(" expr ")")
				                                      t nil)))))
