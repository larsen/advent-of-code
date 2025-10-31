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
  (let* ((new-allocation (alexandria:copy-array allocation))
         (l (length new-allocation))
         (index-to-redistribute (pick-block-index-to-redistribute new-allocation))
         (value-to-redistribute (aref new-allocation index-to-redistribute)))
    ;; to start, we remove the blocks we have to redistribute
    (setf (aref new-allocation index-to-redistribute) 0)
    (loop while (> value-to-redistribute 0)
          for idx from (+ 1 index-to-redistribute)
          for wrapped-idx = (mod idx l)
          do (incf (aref new-allocation wrapped-idx))
             (decf value-to-redistribute))
    new-allocation))

(defun cycle-detector (allocation)
  (loop with cycle-counter = 0
        with encountered-allocations = (make-hash-table :test 'equalp)
        for current-allocation = allocation
          then (redistribute current-allocation)
        when (gethash current-allocation encountered-allocations)
          return (values cycle-counter current-allocation encountered-allocations)
        do (incf cycle-counter)
           (setf (gethash current-allocation encountered-allocations) t)))

(defun aoc2017/day6/solution1 ()
  (cycle-detector (read-block-counts)))

(defun aoc2017/day6/solution2 ()
  (multiple-value-bind (_ allocation)
      (cycle-detector (read-block-counts))
    (declare (ignore _))
    (cycle-detector allocation)))
