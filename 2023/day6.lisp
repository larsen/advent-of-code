(in-package #:advent-of-code)

(defun read-boats-records ()
  (let ((records (mapcar #'parse-integer
                         (all-matches-as-strings "\\d+"
                                                 (read-input-file "inputs/2023/day6")))))
    (transpose (list (subseq records 0 (/ (length records) 2))
                     (subseq records (/ (length records) 2))))))

(defun aoc2023/day6/solution1 ()
  (apply #'*
         (loop for (race-duration record) in (read-boats-records)
               collect (loop for hold from 1 below race-duration
                             count (> (* hold (- race-duration hold)) record)))))

(defun read-boats-records-as-single-race ()
  (let ((records (mapcar #'parse-integer
                         (all-matches-as-strings "\\d+"
                                                 (remove #\Space
                                                  (read-input-file "inputs/2023/day6"))))))
    records))

(defun aoc2023/day6/solution2 ()
  (loop with (race-duration record) = (read-boats-records-as-single-race)
        for hold from 1 below race-duration
        count (> (* hold (- race-duration hold)) record)))
