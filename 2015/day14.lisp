(in-package #:advent-of-code)

(defun read-reindeers ()
  (let ((reindeers (make-hash-table :test #'equal))
        (re "(\\w+) can fly (\\d+) km/s for (\\d+) seconds, but then must rest for (\\d+) seconds."))
    (loop for r in (uiop:read-file-lines (asdf:system-relative-pathname
                                          'advent-of-code "inputs/2015/day14"))
          do (register-groups-bind (name
                                    (#'parse-integer speed)
                                    (#'parse-integer flying-time)
                                    (#'parse-integer rest-time))
                 (re r)
               (setf (gethash name reindeers)
                     (list speed flying-time rest-time)))
          finally (return reindeers))))

(defun reindeer-flying-or-resting (elapsed-time flying-time rest-time)
  (reset-flip-flop)
  (loop with acc = 0
        for state = (flip-flop 'flying 'resting)
        do (incf acc (if (eql state 'resting)
                         rest-time
                         flying-time))
        while (< acc elapsed-time)
        finally (return state)))

(defun aoc2015/day14/solution1 ()
  (let ((reindeers (read-reindeers))
        (reindeers-distance (make-hash-table :test #'equal)))
    (loop for r being the hash-keys of reindeers
          do (setf (gethash r reindeers-distance) 0))
    (loop for elapsed from 1 to 2503
          do (loop for r being the hash-keys of reindeers
                   do (destructuring-bind (speed flying-time rest-time)
                          (gethash r reindeers)
                        (incf (gethash r reindeers-distance)
                              (if (eql 'flying (reindeer-flying-or-resting elapsed
                                                                           flying-time
                                                                           rest-time))
                             speed
                             0))))
          finally (return (apply #'max (hash-table-values reindeers-distance))))))

(defun aoc2015/day14/solution2 ()
  (let ((reindeers (read-reindeers))
        (reindeers-distance (make-hash-table :test #'equal))
        (reindeers-points (make-hash-table :test #'equal)))
    (loop for r being the hash-keys of reindeers
          do (setf (gethash r reindeers-distance) 0)
             (setf (gethash r reindeers-points) 0))
    ;; Race!
    (loop for elapsed from 1 to 2503
          do (loop for r being the hash-keys of reindeers
                   do (destructuring-bind (speed flying-time rest-time)
                          (gethash r reindeers)
                        (incf (gethash r reindeers-distance)
                              (if (eql (reindeer-flying-or-resting
                                        elapsed flying-time rest-time)
                                       'flying)
                                  speed
                                  0))))
             ;; Point attribution
             (loop with max-distance = (apply #'max (hash-table-values reindeers-distance))
                   for r being the hash-keys of reindeers-points
                   do (if (= max-distance (gethash r reindeers-distance))
                          (incf (gethash r reindeers-points))))
          finally (return (apply #'max (hash-table-values reindeers-points))))))
