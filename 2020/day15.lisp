(in-package #:advent-of-code)

(defparameter *day15/input* '(14 1 17 0 3 20))

(defun elves-game (max-turn)
  (let ((memory (make-hash-table))
        (last-spoken nil))
    (loop for n in *day15/input*
          for turn from 1
          do (setf (gethash n memory '())
                   (cons turn nil))
             (setf last-spoken n))
    (loop for turn from (+ 1 (length *day15/input*)) to max-turn
          do (if (not (null (cdr (gethash last-spoken memory))))
                 (progn
                   (let* ((spoken
                           (- (car (gethash last-spoken memory))
                              (cdr (gethash last-spoken memory)))))
                     (setf last-spoken spoken)
                     (setf (gethash last-spoken memory)
                           (cons turn (car (gethash last-spoken memory))))))
                 (progn
                   (setf last-spoken 0)
                   (setf (gethash last-spoken memory)
                         (cons turn (car (gethash last-spoken memory)))))))
    last-spoken))

(defun aoc2020/day15/solution1 ()
  (elves-game 2020))

;; TODO slow!
(defun aoc2020/day15/solution2 ()
  (elves-game 30000000))
