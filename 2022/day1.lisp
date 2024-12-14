(in-package #:advent-of-code)

(defun read-calories-groups ()
  (split-sequence nil (mapcar (lambda (str)
                                (parse-integer str :junk-allowed t))
                              (uiop:read-file-lines
                               (asdf:system-relative-pathname
                                :advent-of-code "inputs/2022/day1")))))

(defun aoc2022/day1/solution1 ()
  (loop for cg in (read-calories-groups)
        maximizing (sum cg)))

;; an alternative technique is to use the function
;; BESTN from Serapeum

(defun aoc2022/day1/solution2 ()
  (sum (subseq
        (sort (mapcar #'sum (read-calories-groups)) #'>)
        0 3)))
