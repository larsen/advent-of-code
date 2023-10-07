(in-package #:advent-of-code)

(defun read-naughty-strings ()
  (uiop:read-file-lines (asdf:system-relative-pathname 'advent-of-code
                                                       "inputs/2015/day5")))

(defun nice-string-p (candidate)
  (and
   ;; It contains at least three vowels
   (>= (count-if (lambda (c) (member c '(#\a #\e #\i #\o #\u)))
                 candidate)
       3)
   ;; It contains at least one letter that appears twice in a row
   (scan "(.)\\1" candidate)
   ;; It does not contain the strings ab, cd, pq, or xy
   (and (not (scan "ab" candidate))
        (not (scan "cd" candidate))
        (not (scan "pq" candidate))
        (not (scan "xy" candidate)))))

(defun aoc2015/day5/solution1 ()
  (count-if #'nice-string-p (read-naughty-strings)))

(defun nicer-nice-string-p (candidate)
  (and
   ;; It contains a pair of any two letters that appears at least
   ;; twice in the string without overlapping
   (scan "(..).*\\1" candidate)
   ;; It contains at least one letter which repeats with exactly one
   ;; letter between them
   (scan "(.).\\1" candidate)
))

(defun aoc2015/day5/solution2 ()
  (count-if #'nicer-nice-string-p (read-naughty-strings)))
