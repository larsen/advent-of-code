(in-package #:advent-of-code)

(defun read-reactor-reports ()
  (loop for l in (read-input-file-as-lines "inputs/2024/day2")
        collect (mapcar #'parse-integer (split "\\s" l))))

(defun gradually-increasing (report)
  (loop for (a b) on report
        while b
        always (<= 1 (- b a) 3)))

(defun gradually-decreasing (report)
  (loop for (a b) on report
        while b
        always (<= 1 (- a b) 3)))

(defun aoc2024/day2/solution1 ()
  (loop for report in (read-reactor-reports)
        count (or (gradually-increasing report)
                  (gradually-decreasing report))))

(defun safe-with-dampener-brute-force (report)
  (loop for i from 0 below (length report)
        for dampened = (append (subseq report 0 i)
                               (subseq report (+ i 1) (length report)))
        thereis (or (gradually-increasing dampened)
                    (gradually-decreasing dampened))))

(defun aoc2024/day2/solution2 ()
  (loop for report in (read-reactor-reports)
        count (or (gradually-increasing report)
                  (gradually-decreasing report)
                  (safe-with-dampener-brute-force report))))
