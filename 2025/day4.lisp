(in-package #:advent-of-code)

(defun is-paper-roll-reachable (x y grid)
  (let* ((directions '((-1 . -1) (0 . -1) (1 . -1)
                       (-1 . 0)           (1 . 0)
                       (-1 . 1)  (0 . 1)  (1 . 1)))
         (reachable (loop for d in directions
                          when (array-in-bounds-p grid
                                                  (+ (car d) x)
                                                  (+ (cdr d) y))
                            count (char= #\@ (aref grid
                                                   (+ (car d) x)
                                                   (+ (cdr d) y))))))
    (< reachable 4)))

(defun remove-reachable-paper-rolls (grid)
  (let ((grid-size (array-dimensions grid)))
    (loop with new-grid = (make-array grid-size)
          for x from 0 below (first grid-size)
          do (loop for y from 0 below (second grid-size)
                   do (setf (aref new-grid x y)
                            (aref grid x y))
                   do (when (and
                             (char= (aref grid x y) #\@)
                             (is-paper-roll-reachable x y grid))
                        (setf (aref new-grid x y) #\.)))
          finally (return new-grid))))

(defun count-reachable (grid)
  (let* ((grid-size (array-dimensions grid))
         (result 0))
    (loop for x from 0 below (first grid-size)
          do (loop for y from 0 below (second grid-size)
                   when (and
                         (char= (aref grid x y) #\@)
                         (is-paper-roll-reachable x y grid))
                     do (incf result))
          finally (return result))))

(defun print-paper-roll-grid (grid)
  (let* ((grid-size (array-dimensions grid)))
    (loop for x from 0 below (first grid-size)
          do (loop for y from 0 below (second grid-size)
                   do (format t "~a" (aref grid x y)))
             (format t "~%"))
    (format t "~%")))

(defun aoc2025/day4/solution1 ()
  (let* ((paper-roll-grid (read-grid "inputs/2025/day4")))
    (count-reachable paper-roll-grid)))

(defun aoc2025/day4/solution2 ()
  (loop for paper-roll-grid = (read-grid "inputs/2025/day4")
          then (remove-reachable-paper-rolls paper-roll-grid)
        while (> (count-reachable paper-roll-grid) 0)
        sum (count-reachable paper-roll-grid)))
