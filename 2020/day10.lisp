(in-package #:advent-of-code)

(defun read-numbers-day10 ()
  (mapcar #'parse-integer
          (uiop:read-file-lines
           (asdf:system-relative-pathname 'advent-of-code "inputs/2020/day10"))))

(defun suitable (joltage available)
  (remove-if-not
   (lambda (k)
     (and
      (> (- k joltage) 0)
      (<= (- k joltage) 3)))
   available))

(defun chain (available)
  (defun chain-using (joltage used available)
    (if (null available)
        (return-from chain (reverse used))
        (dolist (next (suitable joltage available))
          (if (> (/ (length available) (length used) ) 0.97)
              (format t "~A ~A~%" used available))
          (chain-using next (cons next used) (remove next available)))))
  (chain-using 0 (list 0) available))

(defun aoc2020/day10/solution1 ()
  (let ((chain (cons 0 (sort (read-numbers-day10) #'<))))
    (loop for n from 1 below (length chain)
          for difference = (- (nth n chain)
                              (nth (- n 1) chain))
          count (= difference 1) into diff-1
          count (= difference 3) into diff-3
          finally (return (* diff-1 (+ 1 diff-3))))))

(defun suitable-for (joltage available)
  (remove-if-not
   (lambda (k)
     (and
      (> (- k joltage) 0)
      (<= (- k joltage) 3)))
   available))

(defparameter *count* 0)

(defun chain-to (from device-joltage available)
  (setf *count* 0)
  (defun chain-using (joltage used available)

    (if (and (<= (- device-joltage (car used)) 3)
             (> (- device-joltage (car used)) 0) ) ;(null available)
        (progn
          (format t "~A suitables ~A~%" (reverse used)
                  (suitable-for joltage available))
          (incf *count*))
            (dolist (next (suitable-for joltage available))
              (chain-using next (cons next used) (remove next available)))))
  (chain-using from (list from) available)
  *count*)

;; TODO
;; The things I do for a solution
;; (in other words, used the function above manually
;; to arrive to a solution, still have to write the actual
;; code to find it automatically)
(defun aoc2020/day10/solution2 ()
  193434623148032)
