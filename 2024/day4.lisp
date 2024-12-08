(in-package #:advent-of-code)

(defun read-ceres-search ()
  (let* ((raw (read-input-file-as-lines "inputs/2024/day4"))
         (rows (length raw))
         (cols (length (first raw))))
    (make-array (list rows cols) :initial-contents raw)))

(defvar +day4-2024-directions+
  '((-1 . -1) (0 . -1) (+1 . -1)
    (-1 .  0)          (+1 .  0)
    (-1 . +1) (0 . +1) (+1 . +1)))

(defun search-in-grid (grid pos direction str)
  (loop for c across str
        for p = pos then (cons (+ (car p) (cdr direction))
                               (+ (cdr p) (car direction)))
        always (and (array-in-bounds-p grid (car p) (cdr p))
                    (char= (aref grid (car p) (cdr p)) c))))

(defun aoc2024/day4/solution1 ()
  (let* ((grid (read-ceres-search))
         (dims (array-dimensions grid)))
    (loop with counter = 0
          for row from 0 below (first dims)
          do (loop for col from 0 below (second dims)
                   do (incf counter
                            (loop for dir in +day4-2024-directions+
                                  count (search-in-grid
                                         grid (cons row col) dir "XMAS"))))
          finally (return counter))))

(defun horizontal-up (grid pos c)
  (and (char= c (aref grid (- (car pos) 1) (- (cdr pos) 1)))
       (char= c (aref grid (- (car pos) 1) (+ (cdr pos) 1)))))

(defun horizontal-down (grid pos c)
  (and (char= c (aref grid (+ (car pos) 1) (- (cdr pos) 1)))
       (char= c (aref grid (+ (car pos) 1) (+ (cdr pos) 1)))))

(defun vertical-left (grid pos c)
  (and (char= c (aref grid (- (car pos) 1) (- (cdr pos) 1)))
       (char= c (aref grid (+ (car pos) 1) (- (cdr pos) 1)))))

(defun vertical-right (grid pos c)
  (and (char= c (aref grid (- (car pos) 1) (+ (cdr pos) 1)))
       (char= c (aref grid (+ (car pos) 1) (+ (cdr pos) 1)))))

(defun search-xmas (grid pos)
  (let ((row (car pos))
        (col (cdr pos)))
    (and (char= #\A (aref grid row col))
         (or (and (vertical-left grid pos #\M)
                  (vertical-right grid pos #\S))
             (and (vertical-left grid pos #\S)
                  (vertical-right grid pos #\M))
             (and (horizontal-up grid pos #\S)
                  (horizontal-down grid pos #\M))
             (and (horizontal-up grid pos #\M)
                  (horizontal-down grid pos #\S))))))

(defun aoc2024/day4/solution2 ()
  (let* ((grid (read-ceres-search))
         (dims (array-dimensions grid)))
    (loop with counter = 0
          for row from 1 below (- (first dims) 1)
          do (loop for col from 1 below (- (second dims) 1)
                   when (search-xmas grid (cons row col))
                     do (incf counter))
          finally (return counter))))
