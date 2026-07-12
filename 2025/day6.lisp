(in-package #:advent-of-code)

(defun read-math-homework ()
  (let* ((raw (read-input-file-as-lines "inputs/2025/day6"))
         (ops (remove "" (split "\\s+" (car (last raw)))
                      :test #'equal))
         (numbers
           (transpose
            (loop for l in (subseq raw 0 (- (length raw) 1))
                  collect (mapcar #'parse-integer
                                  (remove "" (split "\\s+" l)
                                          :test #'equal))))))
    (loop for i from 0 below (length ops)
          collect (list (if (string= "*" (nth i ops))
                            #'*
                            #'+)
                        (nth i numbers)))))

(defun aoc2025/day6/solution1 ()
  (loop for (func numbers) in (read-math-homework)
        sum (apply func numbers)))

(defun aoc2025/day6/solution2 ()
  )
