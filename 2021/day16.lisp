(in-package #:advent-of-code)

(defun string-to-bit-vector (str)
  (make-array (length str)
              :element-type 'bit
              :initial-contents (mapcar #'digit-char-p (coerce str 'list))))

;; 110100101111111000101000

(defun read-bits-message ()
  (loop for h across (uiop:read-file-string
                      (asdf:system-relative-pathname 'advent-of-code "inputs/2021/day16"))
        append ))
