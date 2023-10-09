(in-package #:advent-of-code)

(defun md5-hex-representation (input index)
  (format nil "~{~A~}"
          (loop for v across (md5:md5sum-string (format nil "~A~A" input index))
                collect (format nil "~2,'0X" v))))

(defun next-password-character (input index)
  (loop for index from index
        for md5 = (md5-hex-representation input index)
        when (string= "00000" (subseq md5 0 5))
          return (cons (nth 5 (coerce md5 'list)) index)))

(defun aoc2016/day5/solution1 (input)
  (let ((index 0)
        (password nil))
    (dotimes (n 8)
      (destructuring-bind (password-character . new-index)
          (next-password-character input index)
        (setf index (+ 1 new-index))
        (setf password (append password (list password-character)))))
    (string-downcase (concatenate 'string password))))

(defun next-password-position-and-character (input index)
  (loop for index from index
        for md5 = (md5-hex-representation input index)
        when (and (string= "00000" (subseq md5 0 5))
                  (digit-char-p (nth 5 (coerce md5 'list)))
                  (< (digit-char-p (nth 5 (coerce md5 'list))) 8)
                  (>= (digit-char-p (nth 5 (coerce md5 'list))) 0))
          return (list (digit-char-p (nth 5 (coerce md5 'list)))
                       (nth 6 (coerce md5 'list))
                       index)))

(defun aoc2016/day5/solution2 (input)
  (let ((index 0)
        (password-characters-guessed 0)
        (password (make-array 8 :element-type 'character :initial-element #\Space)))
    (loop
      do (destructuring-bind (position password-character new-index)
          (next-password-position-and-character input index)
           (setf index (+ 1 new-index))
           (if (char= (aref password position) #\Space)
               (progn
                 (setf (aref password position) password-character)
                 (incf password-characters-guessed))))
      until (= 8 password-characters-guessed))
    (string-downcase (concatenate 'string password))))
