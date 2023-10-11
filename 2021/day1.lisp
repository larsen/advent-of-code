(in-package #:advent-of-code)

(defun read-sonar-sweep ()
  (mapcar #'parse-integer
          (uiop:read-file-lines
           (asdf:system-relative-pathname
            'advent-of-code "inputs/2021/day1"))))

(defun count-increasing-values (numbers)
  (loop for couple in (zip #'cons numbers (cdr numbers))
        when (> (cdr couple)
                (car couple))
          count couple))

;; A much more succinct version from Reddit
;;
;; (defun increasing-pairs (nums)
;;   (loop for (a b) on nums while b count (< a b)))

(defun zip (f &rest lists)
  (apply #'mapcar f lists))

(defun aoc2021/day1/solution1 ()
  (count-increasing-values (read-sonar-sweep)))

(defun aoc2021/day1/solution2 ()
  (let ((sonar-sweeps (read-sonar-sweep)))
    (count-increasing-values
     (zip #'+
          sonar-sweeps
          (cdr sonar-sweeps)
          (cdr (cdr sonar-sweeps))))))
