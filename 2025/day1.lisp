(in-package #:advent-of-code)

(defun read-dial-rotations ()
  (loop for l in (read-input-file-as-lines "inputs/2025/day1")
        collect (register-groups-bind (direction (#'parse-integer steps))
                    ("(.)(\\d+)" l)
                  (list (char direction 0) steps))))

(defun dial-apply-rotation (dial direction steps)
  (let* ((new-direction (+ dial (if (char= #\L direction)
                                    (* -1 steps)
                                    steps)))
         (new-direction-wrapped (mod new-direction 100)))
    (values new-direction-wrapped
            new-direction)))

(defun dial-apply-rotation2 (dial direction steps)
  (multiple-value-bind (full-rotations wsteps)
      (floor steps 100)
    (let* ((new-direction (+ dial (if (char= #\L direction)
                                      (* -1 wsteps)
                                      wsteps)))
           (new-direction-wrapped (mod new-direction 100)))
      (values new-direction-wrapped
              new-direction
              full-rotations))))

(defun aoc2025/day1/solution1 ()
  (loop with rotations = (read-dial-rotations)
        with p = 50
        with password = 0
        for (direction steps) in rotations
        do (setf p (dial-apply-rotation p direction steps))
        when (= p 0)
          do (incf password)
        finally (return password)))

(defun aoc2025/day1/solution2 ()
  (loop with rotations = (read-dial-rotations)
        with p = 50
        with password = 0
        for (direction steps) in rotations
        do (multiple-value-bind (new-direction-wrapped
                                 new-direction
                                 full-rotations)
               (dial-apply-rotation2 p direction steps)
             (setf p new-direction-wrapped)
             (incf password full-rotations)
             (when (or (not (= new-direction new-direction-wrapped))
                       (= p 0))
               (incf password)))
         finally (return password)))
