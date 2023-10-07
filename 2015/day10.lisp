(in-package #:advent-of-code)

(defun look-and-say-slow (digits-string)
  (declare (type (simple-string) digits-string)
           (optimize (speed 3) (safety 0) (debug 0)))
  (let ((result "")
        (last-digit)
        (subseq-counter 0))
    (declare (fixnum subseq-counter))
    (loop for d across digits-string
          do (if (not last-digit)
                 (setf last-digit d
                       subseq-counter 0))
             (if (char-not-equal d last-digit)
                 (setf result (concatenate 'string result
                                           (format nil "~d~c" subseq-counter last-digit))
                       last-digit d
                       subseq-counter 1)
                 (incf subseq-counter))
          finally (return (concatenate 'string result
                                       (format nil "~d~c" subseq-counter last-digit))))))

;; alternative implementation using lists is much faster
;; (https://github.com/HenryS1/adventofcode/blob/master/2015/day10.lisp)

(defun look-and-say (ns)
  (loop with count = 1
     for prev = nil then curr
     for rest = ns then (cdr rest)
     for curr = (car rest)
     if (and prev curr (= prev curr))
     do (incf count)
     else if prev
     collect count and
     collect prev and
     do (setf count 1)
     while rest))

(defun aoc2015/day10/solution1 ()
  (loop with str = '(3 1 1 3 3 2 2 1 1 3)
        repeat 40
        do (setf str (look-and-say str))
        finally (return (length str))))

(defun aoc2015/day10/solution2 ()
  (loop with str = '(3 1 1 3 3 2 2 1 1 3)
        repeat 50
        do (setf str (look-and-say str))
        finally (return (length str))))
