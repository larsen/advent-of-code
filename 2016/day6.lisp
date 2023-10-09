(in-package #:advent-of-code)

(defun read-jammed-communication ()
  (read-input-file-as-lines "inputs/2016/day6"))

(defun compute-column-char-frequency (input column)
  (let ((freq-table (make-hash-table)))
    (loop for line in input
          do (incf (gethash (aref line column) freq-table 0))
          finally (return freq-table))))

(defun extract-frequent-character (input column &key least-frequent)
  (let ((comparison-f (if least-frequent
                          #'<=
                          #'>=))
        (freq-table (compute-column-char-frequency input column)))
    (car (first
          (sort (loop for k being the hash-keys of freq-table
                    using (hash-value v)
                  collect (cons k v))
            comparison-f :key #'cdr)))))

(defun most-frequent-character (input column)
  (extract-frequent-character input column))

(defun least-frequent-character (input column)
  (extract-frequent-character input column :least-frequent t))

(defun aoc2016/day6/solution1 ()
  (let* ((jammed-communication (read-jammed-communication))
         (message-length (length (first jammed-communication))))
    (concatenate 'string
                 (loop for col from 0 to (- message-length 1)
                       collect (most-frequent-character jammed-communication col)))))

(defun aoc2016/day6/solution2 ()
  (let* ((jammed-communication (read-jammed-communication))
         (message-length (length (first jammed-communication))))
    (concatenate 'string
                 (loop for col from 0 to (- message-length 1)
                       collect (least-frequent-character jammed-communication col)))))
