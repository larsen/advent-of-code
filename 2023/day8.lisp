(in-package #:advent-of-code)

(defun read-desert-island-maps ()
  (let ((file-content (read-input-file "inputs/2023/day8"))
        (desert-map (make-hash-table :test #'equal)))
    (destructuring-bind (lr-instructions nodes)
        (split "\\n\\n" file-content)
      (dolist (l (split "\\n" nodes))
        (register-groups-bind (start left right)
            ("(\\w{3}) = \\((\\w{3})\\, (\\w{3})\\)" l)
          (setf (gethash start desert-map)
                (list left right))))
      (values lr-instructions
              desert-map))))

(defun aoc2023/day8/solution1 ()
  (multiple-value-bind (lr-instructions desert-map)
      (read-desert-island-maps)
    (loop with s = (make-instance 'wrapped-sequence
                                  :sequence (coerce lr-instructions 'list))
          with pos = "AAA"
          with steps = 0
          for instr = (next s)
          do (incf steps)
             (cond
               ((char= #\L instr) (setf pos (car (gethash pos desert-map))))
               ((char= #\R instr) (setf pos (cadr (gethash pos desert-map)))))
          until (string= pos "ZZZ")
          finally (return steps))))

(defun aoc2023/day8/solution2 ()
  ;; SLOW!
  #+nil
  (multiple-value-bind (lr-instructions desert-map)
      (read-desert-island-maps)
    (loop with s = (make-instance 'wrapped-sequence
                                  :sequence (coerce lr-instructions 'list))
          with poss = (loop for k being the hash-keys of desert-map
                            when (char= #\A (elt k 2))
                              collect k)
          with steps = 0
          for instr = (next s)
          do (incf steps)
             (setf poss
                   (mapcar (lambda (pos)
                             (cond
                               ((char= #\L instr) (car (gethash pos desert-map)))
                               ((char= #\R instr) (cadr (gethash pos desert-map)))))
                           poss))
          until (loop for pos in poss
                      always (char= #\Z (elt pos 2)))
          finally (return steps))))
