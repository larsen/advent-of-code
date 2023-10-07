(in-package #:advent-of-code)

(defun read-containers ()
  (loop for line in (uiop:read-file-lines
                     (asdf:system-relative-pathname 'advent-of-code
                                                    "inputs/2015/day17"))
        collect (parse-integer line)))

(defun count-change (total available-containers)
  (cond
    ((= total 0) 1)
    ((< total 0) 0)
    ((null available-containers) 0)
    (t (+ (count-change (- total (car available-containers)) (cdr available-containers))
          (count-change total (cdr available-containers))))))

;; Should be 4
(defun aoc2015/day17/solution1-test ()
  (count-change 25 '(20 15 10 5 5)))

(defun aoc2015/day17/solution1 ()
  (count-change 150 (read-containers)))

(defvar +day17-solutions-by-number-of-containers+ (make-hash-table))

;; Count solutions, and keep count of the # of containers used
(defun count-change2 (total available-containers used)
  (cond
    ((= total 0)
     (incf (gethash used +day17-solutions-by-number-of-containers+ 0))
     1)
    ((< total 0) 0)
    ((null available-containers) 0)
    (t (+ (count-change2 (- total (car available-containers))
                         (cdr available-containers)
                         (+ used 1))
          (count-change2 total
                         (cdr available-containers)
                         used)))))

(defun aoc2015/day17/solution2 ()
  (setf +day17-solutions-by-number-of-containers+ (make-hash-table))
  (count-change2 150 (read-containers) 0)
  (loop for k being the hash-keys of +day17-solutions-by-number-of-containers+
        minimize k into minimum-number-of-containers
        finally (return (gethash minimum-number-of-containers
                                 +day17-solutions-by-number-of-containers+))))
