(in-package #:advent-of-code)

(defun read-bus-schedules ()
  (let ((input (uiop:read-file-lines
                (asdf:system-relative-pathname 'advent-of-code
                                               "inputs/2020/day13"))))
    (cons (parse-integer (car input))
          (list
           (mapcar #'parse-integer
                   (remove-if (lambda (s)
                                (string= s "x"))
                              (split-sequence #\, (cadr input)))))
          )))

(defun aoc2020/day13/solution1 ()
  (destructuring-bind (earliest-time buses)
      (read-bus-schedules)
    (let ((best-solution
            (first (sort (mapcar (lambda (lst)
                                   (car (reverse lst)))
                                 (loop for b in buses
                                       collect (loop for sched from 0 by b
                                                     collect (cons b (- sched earliest-time))
                                                     until (> sched earliest-time))))
                         #'< :key #'cdr))))
      (* (car best-solution)
         (cdr best-solution)))))

;; TODO
;; Solution found "manually": performing calculations in the REPL
;; after reducing the problem a bit.
;; I need to write a programmatic solver
(defun aoc2020/day13/solution2 ()
  379786358533423)
