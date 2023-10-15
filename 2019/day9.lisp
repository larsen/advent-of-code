(in-package #:advent-of-code)

(defun aoc2019/day9/solution1 ()
  (let ((program (intcode-read-program (asdf:system-relative-pathname
                                        'advent-of-code "inputs/2019/day9")))
        (cpu (make-instance 'cpu :input '(1))))
    (setf (mem cpu) program)
    (run! cpu)
    (first (output cpu))))

(defun aoc2019/day9/solution2 ()
  (let ((program (intcode-read-program (asdf:system-relative-pathname
                                        'advent-of-code "inputs/2019/day9")))
        (cpu (make-instance 'cpu :input '(2))))
    (setf (mem cpu) program)
    (run! cpu)
    (first (output cpu)))  )
