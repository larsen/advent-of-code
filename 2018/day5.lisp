(in-package #:advent-of-code)

(defun read-polymer ()
  (string-right-trim '(#\Newline)
                     (uiop:read-file-string (asdf:system-relative-pathname
                                             :advent-of-code "inputs/2018/day5"))))

(defun opposite-polarity (c1 c2)
  (and (not (char= c1 c2))
       (char= (char-downcase c1) (char-downcase c2))))

(defun reactions (polymer)
  (let ((polymer-as-list (coerce polymer 'list))
        (index 0))
    (loop for (c1 c2) on polymer-as-list
          until (and c2 (opposite-polarity c1 c2))
          do (incf index))
    (if (< index (length polymer))
        (concatenate 'string
                     (subseq polymer 0 index)
                     (subseq polymer (+ index 2)))
        polymer)))

(defun fixed-point (f x0 &key (test #'eql))
  "Returns the fixed point of function F, starting from X0.
That is, the value X such that F(X) = X. It assumes F converges
in given initial condition"
  (loop for x = x0 then (funcall f x1)
        for x1 = (funcall f x)
        until (funcall test x x1)
        finally (return x)))

(defun aoc2018/day5/solution1 ()
  (length (fixed-point #'reactions (read-polymer) :test #'string=)))

(defun aoc2018/day5/solution2 ()
  (let ((polymer (read-polymer)))
    (loop for fawlty-type-code from 65 to (+ 65 25)
          for fawlty-type = (code-char fawlty-type-code)
          for p = (remove fawlty-type (remove (char-downcase fawlty-type) polymer))
          minimize (length (fixed-point #'reactions p :test #'string=)))))
