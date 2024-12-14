(in-package :advent-of-code)

(defun read-block-counts ()
  (coerce
   (loop for b in (split-sequence:split-sequence
                   #\Tab (uiop:read-file-string
                          (asdf:system-relative-pathname 'advent-of-code
                                                         "inputs/2017/day6")))
         collect (parse-integer b))
   'vector))

(defun pick-block-index-to-redistribute (allocation)
  (loop with max-index = 0
        with max = 0
        for index from 0 below (length allocation)
        do (when (> (aref allocation index) max)
             (setf max (aref allocation index))
             (setf max-index index))
        finally (return max-index)))

(defun redistribute (allocation)
  (let* ((l (length allocation))
         (index-to-redistribute (pick-block-index-to-redistribute allocation))
         (value-to-redistribute (aref allocation index-to-redistribute)))
    ;; to start, we remove the blocks we have to redistribute
    (setf (aref allocation index-to-redistribute) 0)
    (loop while (> value-to-redistribute 0)
          for idx from (+ 1 index-to-redistribute)
          for wrapped-idx = (mod idx l)
          do (incf (aref allocation wrapped-idx))
             (decf value-to-redistribute))
    allocation))

(defun aoc2017/day6/solution1 ()
  (loop with cycle-counter = 0
        with cycle-detector = (make-hash-table :test 'equalp)
        for allocation = (read-block-counts)
          then (redistribute allocation)
        do (incf cycle-counter)
        when (gethash allocation cycle-detector)
          return (- cycle-counter 1)
        do (setf (gethash allocation cycle-detector) t)))

(defun cycle-detector (allocation)
  (loop with cycle-counter = 0
        with cycle-detector = (make-hash-table :test 'equalp)
        for current-allocation = (read-block-counts)
          then (redistribute allocation)
        do (incf cycle-counter)
        when (gethash current-allocation cycle-detector)
          return (values (- cycle-counter 1)
                         current-allocation)
        do (setf (gethash current-allocation cycle-detector) t)))

(defun aoc2017/day6/solution1 ()
  (cycle-detector (read-block-counts)))

(defun aoc2017/day6/solution2 ()
  (multiple-value-bind (_ allocation)
      (cycle-detector (read-block-counts))
    (declare (ignore _))
    (- (cycle-detector allocation) 1)))
