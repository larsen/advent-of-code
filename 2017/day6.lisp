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
        do (if (> (aref allocation index) max)
               (progn (setf max (aref allocation index))
                      (setf max-index index)))
        finally (return max-index)))

(defun redistribute (allocation)
  (let* ((l (length allocation))
         (index-to-redistribute (pick-block-index-to-redistribute allocation))
         (value-to-redistribute (aref allocation index-to-redistribute))
         (individual-redistribution (floor (/ value-to-redistribute
                                              (- l 1))))
         (reminder (- value-to-redistribute (* (- l 1) individual-redistribution))))
    (incf (aref allocation index-to-redistribute) reminder)
    (loop for idx from 0 below l
          when (/= idx index-to-redistribute)
            do (incf (aref allocation idx) individual-redistribution))
    allocation))

(defun aoc2017/day6/solution1 ()
  (loop with cycle-counter = 0
        with cycle-detector = (make-hash-table :test 'equalp)
        for allocation = #(0 2 7 0) ;; (read-block-counts)
          then (redistribute allocation)
        do (incf cycle-counter)
        when (gethash allocation cycle-detector)
          return cycle-counter
        do (setf (gethash allocation cycle-detector) t)))

(defun aoc2017/day6/solution2 ()
  )
