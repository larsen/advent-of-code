(in-package #:advent-of-code)

(defun read-replacements-and-molecule ()
  (let ((replacements (make-hash-table :test #'equal))
        (molecule))
    (loop with mode = :reading-replacements
          for line in (uiop:read-file-lines
                       (asdf:system-relative-pathname 'advent-of-code
                                                      "inputs/2015/day19"))
          if (emptyp line)
            do (setf mode :reading-molecule)
          do (if (eql mode :reading-molecule)
                 (setf molecule line)
                 (register-groups-bind (molecule replacement)
                     ("(\\w+) => (\\w+)" line)
                   (push replacement
                         (gethash molecule replacements '())))))
    (values replacements
            molecule)))

(defun aoc2015/day19/solution1 ()
  (let ((resulting-molecules (make-hash-table :test #'equal)))
    (multiple-value-bind (replacements molecule)
        (read-replacements-and-molecule)
      (loop for substring being the hash-keys of replacements
            do (loop for possible-replacement in (gethash substring replacements)
                     do (do-matches (s e substring molecule)
                          (incf (gethash (replace-substr molecule s e possible-replacement)
                                         resulting-molecules 0))))
            finally (return (length (hash-table-keys resulting-molecules)))))))

(defparameter +minimum-steps+ 1000000)
(defparameter +molecules-encountered+ 0)
(defparameter +h-molecules-encountered+ (make-hash-table :test #'equal))

(defun count-steps-to-target-molecule (molecule replacements target-molecule steps)
  ;; Substitution rules can only make the molecule longer
  ;; or the same length
  (incf +molecules-encountered+)
  (cond
    ;; We already encountered this molecule
    ;; no point going further
    ((gethash molecule +h-molecules-encountered+)
     nil)
    ;; The molecule matches the target:
    ;; if reached in fewer steps, update the record
    ((string= molecule target-molecule)
     (if (< steps +minimum-steps+)
         (setf +minimum-steps+ steps)))
    ;; We made more steps than the minimum to reach a solution
    ;; or, the molecule is longer than the target
    ;; no point going further
    ((or (> steps +minimum-steps+)
         (> (length molecule)
            (length target-molecule)))
     (progn
       (incf (gethash molecule +h-molecules-encountered+ 0))
       nil))
    ;; Otherwise, let's try other substitutions recursively
    (t (progn
         (incf (gethash molecule +h-molecules-encountered+ 0))
         (loop for substring being the hash-keys of replacements
                    do (do-matches (s e substring molecule)
                         (loop for possible-replacement in (gethash substring replacements)
                               ;; Recursive call
                               do (count-steps-to-target-molecule
                                   (replace-substr molecule s e possible-replacement)
                                   replacements
                                   target-molecule
                                   (+ 1 steps)))))))))

(defun aoc2015/day19/solution2 ()
  (setf +minimum-steps+ 10000000)
  (setf +molecules-encountered+ 0)
  (setf +h-molecules-encountered+ (make-hash-table :test #'equal))
  (multiple-value-bind (replacements target-molecule)
      (read-replacements-and-molecule)
    (count-steps-to-target-molecule "e" replacements target-molecule 0))
  +minimum-steps+)

(defun aoc2015/day19/solution2-test ()
  (setf +minimum-steps+ 1000000)
  (setf +molecules-encountered+ 0)
  (setf +h-molecules-encountered+ (make-hash-table :test #'equal))
  (let ((target-molecule "HOHOHO")
        (replacements (make-hash-table :test #'equal)))
    (setf (gethash "e" replacements) '("H" "O"))
    (setf (gethash "H" replacements) '("HO" "OH"))
    (setf (gethash "O" replacements) '("HH"))
    (count-steps-to-target-molecule "e" replacements target-molecule 0)
    +minimum-steps+))
