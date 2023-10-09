(in-package #:advent-of-code)

(defun aoc2019/day2/solution1 ()
  (let ((program (read-program (asdf:system-relative-pathname
                                'advent-of-code "inputs/2019/day2"))))
    (setf (aref program 1) 12)
    (setf (aref program 2) 2)
    (run-program program)
    (aref program 0)))

;; UGLY
(defun aoc2019/day2/solution2 ()
  (let ((original-program (read-program (asdf:system-relative-pathname
                                         'advent-of-code "inputs/2019/day2")))
        (program nil)
        (res-noun nil)
        (res-verb nil))
    (loop for noun from 1 to 100
          collect (loop for verb from 1 to 100
                        do (setf program (alexandria:copy-array original-program))
                           (setf (aref program 1) noun)
                           (setf (aref program 2) verb)
                           (run-program program)
                        when (= (aref program 0) 19690720)
                          do (setf res-noun noun)
                             (setf res-verb verb)))
    (+ (* 100 res-noun) res-verb)))
