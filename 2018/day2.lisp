(in-package #:advent-of-code)

(defun read-boxes-id ()
  (uiop:read-file-lines (asdf:system-relative-pathname :advent-of-code "inputs/2018/day2")))

(defun make-char-freq-table (str)
  (let ((freq-table (make-hash-table :test #'equal)))
    (loop for c across str
          do (incf (gethash c freq-table 0))
          finally (return freq-table))))

(defun exactly-n-of-any-letter (n str)
  (loop for k being the hash-values of (make-char-freq-table str)
        thereis (= k n)))

(defun count-exactly-n-of-any-letter (n strings)
  (loop for s in strings
        count (exactly-n-of-any-letter n s) into res
        finally (return res)))

(defun count-exactly-three-of-any-letter (ids)
  (count-exactly-n-of-any-letter 3 ids))

(defun count-exactly-two-of-any-letter (ids)
  (count-exactly-n-of-any-letter 2 ids))

(defun aoc2018/day2/solution1 ()
  (let ((ids (read-boxes-id)))
    (* (count-exactly-two-of-any-letter ids)
       (count-exactly-three-of-any-letter ids))))

;; ----

(defun string-diff (str1 str2)
  (loop for c1 across str1
        for c2 across str2
        when (char/= c1 c2)
          collect c1))

(defun remove-diff (str1 str2)
  (coerce (loop for c1 across str1
                for c2 across str2
                when (char= c1 c2)
                  collect c1)
          'string))

(defun aoc2018/day2/solution2 ()
  (block outer
    (loop with ids = (read-boxes-id)
          for s1 in ids
          do (loop for s2 in ids
                   for diff = (string-diff s1 s2)
                   when (= 1 (length diff))
                     do (return-from outer
                          (remove-diff s1 s2))))))
