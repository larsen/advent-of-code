(in-package #:advent-of-code)

(defun parse-packet (str)
  (read-from-string
   (loop for (from . to) in '(("\\[" "(")
                              ("\\]" ")")
                              ("," " "))
         do (setf str (regex-replace-all from str to))
         finally (return str))))

(defparameter +packet-input-file+ "inputs/2022/day13")

(defun read-packets-pairs ()
  (loop for (l r) in (split-sequence ""
                                     (uiop:read-file-lines
                                      (asdf:system-relative-pathname
                                       :advent-of-code +packet-input-file+))
                                     :test #'string=)
        collect (list (parse-packet l) (parse-packet r))))

(defun read-all-packets ()
  (mapcar #'parse-packet (remove "" (uiop:read-file-lines
                                     (asdf:system-relative-pathname
                                      :advent-of-code +packet-input-file+))
                                 :test #'string=)))

(defun packet< (left right)
  (cond
    ;; If both values are integers…
    ((and (numberp left) (numberp right))
     (< left right))

    ;; If exactly one value is an integer…
    ((and (numberp left) (listp right))
     (packet< (list left) right))
    ((and (listp left) (numberp right))
     (packet< left (list right)))

    ((null left) t)
    ((null right) nil)

    ;; If both values are lists…
    ((and (listp left) (listp right))
     (if (equal (car left) (car right))
         (packet< (cdr left) (cdr right))
         (packet< (car left) (car right))))))

(defun aoc2022/day13/solution1 ()
  (loop for index from 1
        for (left right) in (read-packets-pairs)
        when (packet< left right)
          sum index))

(defun aoc2022/day13/solution2 ()
  (apply #'* (loop for index from 1
                   for packet in (sort (append (read-all-packets)
                                               '(((2)))
                                               '(((6)))) #'packet<)
                   when (member packet '(((2))
                                         ((6)))
                                :test #'equal)
                     collect index)))
