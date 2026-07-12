(in-package #:advent-of-code)

(defun read-battery-banks ()
  (let ((raw (read-input-file-as-lines "inputs/2025/day3")))
    (loop for l in raw
          collect (loop for c across l
                        collect (digit-char-p c)))))

(defun max-and-position (lst)
  (loop with max = 0
        with pos = -1
        with max-position = pos
        for x in lst
        do (incf pos)
        when (> x max)
          do (setq max x)
             (setq max-position pos)
        finally (return (list max max-position))))

(defun aoc2025/day3/solution1 ()
  (let ((banks (read-battery-banks)))
    (loop for b in banks
          for (m1 p1) = (max-and-position (subseq b 0 (- (length b) 1)))
          for (m2 p2) = (max-and-position (subseq b (+ 1 p1) (length b)))
          sum (+ (* m1 10) m2))))

(defun aoc2025/day3/solution2 ()
  )
