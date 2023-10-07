(in-package #:advent-of-code)

(defun successor (c)
  "Returns the character following C in alphabetic order. If the
character is the last one of the alphabet, returns a carry flag
represented as a boolean."
  (if (char= c #\z)
      (values #\a t)
      (values (code-char (1+ (char-code c)))
              nil)))

(defun inc-password (password)
  (loop with carry = nil
        for position = (1- (length password)) then (1- position)
        for c = (char password position)
        do (multiple-value-bind (succ crry)
               (successor c)
             (setf (char password position) succ)
             (setf carry crry))
        until (not carry)
        finally (return password)))

(defun valid-password-p (password)
  (and
   ;; must include one increasing straight of at least three letters
   (scan +big-re+ password)
   ;; may not contain the letters i, o, or l
   (not (scan "i|o|l" password))
   ;; must contain at least two different, non-overlapping pairs of
   ;; letters
   (multiple-value-bind (matched-string letters)
       (scan-to-strings "(.)\\1.*(.)\\2" password)
     (declare (ignore matched-string))
     (if letters
       (not (string= (aref letters 0)
                     (aref letters 1)))
       nil))))

(defun big-re ()
  (format nil "~{~a~^|~}"
        (mapcar (lambda (triplet)
                  (funcall #'concatenate 'string triplet))
                (loop for c = #\a then (successor c)
                      collect (list c (successor c) (successor (successor c)))
                      until (char= c #\x)))))

(defvar +big-re+)

(defun aoc2015/day11/solution1 ()
  (unless (boundp '+big-re+)
    (setf +big-re+ (big-re)))
  (loop for password = "hepxcrrq"
        do (setf password (inc-password password))
        while (not (valid-password-p password))
        finally (return password)))

(defun aoc2015/day11/solution2 ()
  (unless +big-re+
    (setf +big-re+ (big-re)))
  (loop for password = "hepxxyzz"
        do (setf password (inc-password password))
        while (not (valid-password-p password))
        finally (return password)))
