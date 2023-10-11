(in-package #:advent-of-code)

(defun read-numbers ()
  (mapcar #'parse-integer (uiop:read-file-lines
                           (asdf:system-relative-pathname
                            'advent-of-code "inputs/2020/day9"))))

(defun checksum (n previous)
  (let (res)
    (map-combinations (lambda (ns)
                        (if (= n (apply #'+ ns))
                            (setf res t)))
                      previous :length 2)
    res))

(defun aoc2020/day9/solution1 ()
  (let ((lst (read-numbers)))
    (loop for idx from 25 below (length lst)
          when (not (checksum (nth idx lst)
                              (subseq lst (- idx 25) idx)))
            return (nth idx lst))))

(defun aoc2020/day9/solution2 ()
  (let* ((lst (read-numbers))
         (previous-result (day9/solution1))
         (solution
           (loop for n from 2 below (- (length lst) 2)
                 append (loop for idx from 0 below (- (length lst) n)
                               when (= previous-result
                                       (apply #'+ (subseq lst idx (+ idx n))))
                                 return (subseq lst idx (+ idx n))))))
    (+ (apply #'min solution)
       (apply #'max solution))))
