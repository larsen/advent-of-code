(in-package #:advent-of-code)

(defun read-digits-sequence ()
  (map 'list (lambda (c)
               (- (char-int c) 48))
       (remove-if-not #'digit-char-p
                      (uiop:read-file-string
                       (asdf:system-relative-pathname 'advent-of-code "inputs/2017/day1")))))

(defun pairs-circular (lst)
  (mapcar #'list lst (append (cdr lst) (list (car lst)))))

(defun aoc2017/day1/solution1 ()
  (let ((digits (read-digits-sequence)))
    (reduce #'+
            (mapcar #'car
                    (remove-if-not (lambda (p)
                                     (= (first p) (second p)))
                                   (pairs-circular digits))))))

(defun pairs-skip (lst)
  (let* ((len (length lst))
         (skip (/ len 2)))
    (loop for n from 0 below len
          collect (cons (nth n lst)
                        (nth (mod (+ n skip) len) lst)))))

(defun aoc2017/day1/solution2 ()
  (let ((digits (read-digits-sequence)))
    (reduce #'+
            (mapcar #'car
                    (remove-if-not (lambda (p)
                                     (= (car p) (cdr p)))
                                   (pairs-skip digits))))))
