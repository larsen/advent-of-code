(in-package #:advent-of-code)

(defun aoc2019/day7/solution1 ()
  (let ((program (intcode-read-program (asdf:system-relative-pathname
                                        'advent-of-code "inputs/2019/day7"))))
    (loop with max-signal = 0
          with max-signal-phases = nil
          for (ph0 ph1 ph2 ph3 ph4) in (permutations (iota 5))
          for ampA-output = (run-and-get-output! program (list ph0 0))
          for ampB-output = (run-and-get-output! program (list ph1 ampA-output))
          for ampC-output = (run-and-get-output! program (list ph2 ampB-output))
          for ampD-output = (run-and-get-output! program (list ph3 ampC-output))
          for ampE-output = (run-and-get-output! program (list ph4 ampD-output))
          when (> ampE-output max-signal)
            do (setf max-signal ampE-output)
               (setf max-signal-phases (list ph0 ph1 ph2 ph3 ph4))
          finally (return max-signal))))

(defun aoc2019/day7/solution2 ()
  (let ((program (intcode-read-program (asdf:system-relative-pathname
                                        'advent-of-code "inputs/2019/day7-test2")))
        (ampA (make-instance 'cpu))
        (ampB (make-instance 'cpu))
        (ampC (make-instance 'cpu))
        (ampD (make-instance 'cpu))
        (ampE (make-instance 'cpu)))
    ;; Load programs
    (loop for c in (list ampA ampB ampC ampD ampE)
          do (setf (mem c) program))
    ;; Main loop
    (loop
          until (and (halt ampA)
                     (halt ampB)
                     (halt ampC)
                     (halt ampD)
                     (halt ampE)))
    )
  )
