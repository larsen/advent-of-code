(in-package #:advent-of-code)

(defun parse-log-ts (ts)
  (local-time:parse-timestring (concatenate 'string ts ":00")
                               :date-time-separator #\Space))

(defun read-overnight-sleep-shift ()
  (loop for l in (uiop:read-file-lines
                  (asdf:system-relative-pathname
                   :advent-of-code "inputs/2018/day4"))
        collect (register-groups-bind
                    (ts action)
                    ("\\[(.*)\\] (.*)" l)
                  (list (parse-log-ts ts) action))))

(defun symbolic-overnight-sleep-shift ()
  (labels ((cmp-shift-log-entries (s1 s2)
             (local-time:timestamp< (first s1) (first s2))))
    (loop with res = '()
          with current-guard = nil
          for (ts action) in (sort (read-overnight-sleep-shift)
                                   #'cmp-shift-log-entries)
          when (scan "falls asleep" action)
            collect (list current-guard :asleep ts)
          when (scan "wakes up" action)
            collect (list current-guard :up ts)
          when (scan "Guard " action)
            do (multiple-value-bind (matched-string groups)
                   (scan-to-strings "Guard #(\\d+) begins shift" action)
                 (declare (ignore matched-string))
                 (setf current-guard (parse-integer (aref groups 0)))))))

(defun group-shifts-by-guard (night-shifts)
  (let ((grouped-shifts (make-hash-table)))
    (loop for (g action ts) in night-shifts
          do (push (list action ts)
                   (gethash g grouped-shifts '())))
    (loop for guard being the hash-keys of grouped-shifts
          do (setf (gethash guard grouped-shifts)
                   (partition (sort (gethash guard grouped-shifts)
                                    (lambda (a1 a2)
                                      (local-time:timestamp<
                                       (second a1) (second a2)))) 2)))
    grouped-shifts))

(defun most-sleeping-guard (night-shifts)
  (let ((grouped-shifts (group-shifts-by-guard night-shifts)))
    (loop with max-time-asleep = 0
          with max-guard = nil
          with max-guard-shifts = nil
          for guard being the hash-keys of grouped-shifts
          for shifts being the hash-values of grouped-shifts
          for time-asleep = (loop for (asleep up) in shifts
                                  sum (/ (local-time:timestamp-difference
                                          (second up) (second asleep))
                                         60))
          when (> time-asleep max-time-asleep)
            do (setf max-time-asleep time-asleep)
               (setf max-guard guard)
               (setf max-guard-shifts shifts)
          finally (return (values max-guard max-guard-shifts)))))

(defun most-slept-minute (shifts)
  (let* ((counter (make-hash-table)))
    (loop for (asleep up) in shifts
          do (loop for start = (second asleep)
                     then (local-time:timestamp+ start 1 :minute)
                   until (local-time:timestamp= start (second up))
                   do (incf (gethash (local-time:timestamp-minute start) counter 0))))
    (first (sort
            (loop for m being the hash-keys of counter
                  for c being the hash-values of counter
                  collect (cons m c))
            #'> :key #'cdr))))

(defun aoc2018/day4/solution1 ()
  (let ((night-shifts (symbolic-overnight-sleep-shift)))
    (multiple-value-bind (guard shifts)
        (most-sleeping-guard night-shifts)
      (* guard (car (most-slept-minute shifts))))))

(defun aoc2018/day4/solution2 ()
  (let* ((night-shifts (symbolic-overnight-sleep-shift))
         (grouped-by-guard (group-shifts-by-guard night-shifts)))
    (loop with most-slept-counter = 0
          with most-slept-minute-guard = nil
          with most-slept-minute = nil
          for g being the hash-keys of grouped-by-guard
          for (m . c) = (most-slept-minute (gethash g grouped-by-guard))
          when (> c most-slept-counter)
            do (setf most-slept-counter c)
               (setf most-slept-minute m)
               (setf most-slept-minute-guard g)
          finally (return (* most-slept-minute-guard most-slept-minute)))))
