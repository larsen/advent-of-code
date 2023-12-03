(in-package #:advent-of-code)

(defun read-calibration-values ()
  (uiop:read-file-lines (asdf:system-relative-pathname
                         'advent-of-code
                         "inputs/2023/day1")))

(defparameter +digit-representations+
  '(("one" . 1)
    ("two" . 2)
    ("three" . 3)
    ("four" . 4)
    ("five" . 5)
    ("six" . 6)
    ("seven" . 7)
    ("eight" . 8)
    ("nine" . 9)
    ("1" . 1)
    ("2" . 2)
    ("3" . 3)
    ("4" . 4)
    ("5" . 5)
    ("6" . 6)
    ("7" . 7)
    ("8" . 8)
    ("9" . 9)))

(defparameter +digit-re+
  (format nil "(~{~a~^|~})"
          (mapcar #'first +digit-representations+)))

(defparameter +digit-re-rev+
  (format nil "(~{~a~^|~})"
          (mapcar (lambda (c)
                    (reverse (first c)))
                  +digit-representations+)))

(defun digits-in-string (str)
  (remove nil (loop for c across str
                    collect (digit-char-p c))))

(defun recover-calibration-value (str)
  (let ((ds (digits-in-string str)))
    (+ (* 10 (first ds))
       (car (last ds)))))

(defun aoc2023/day1/solution1 ()
  (loop for line in (read-calibration-values)
        summing (recover-calibration-value line)))

(defun first-number (str)
  (cdr (assoc (first (all-matches-as-strings +digit-re+ str))
              +digit-representations+
              :test 'equal)))

(defun last-number (str)
  (cdr (assoc (reverse
               (first (all-matches-as-strings +digit-re-rev+
                                              (reverse str))))
              +digit-representations+
              :test 'equal)))

(defun recover-calibration-value2 (str)
  (+ (* 10 (first-number str))
     (last-number str)))

(defun aoc2023/day1/solution2 ()
  (loop for line in (read-calibration-values)
        summing (recover-calibration-value2 line)))
