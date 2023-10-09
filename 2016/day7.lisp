(in-package #:advent-of-code)

(defun read-ip-addresses-list ()
  (read-input-file-as-lines "inputs/2016/day7"))

(defun proper-palindrome? (string)
  (assert (= (length string) 4))
  (register-groups-bind (c1 c2)
      ("(.)(.)\\2\\1" string)
    (not (string= c1 c2))))

(defun proper-palindrome-3? (string)
  (assert (= (length string) 3))
  (and (not (or (scan "\\[" string)
                (scan "\\]" string)))
       (register-groups-bind (c1 c2)
           ("(.)(.)\\1" string)
         (not (string= c1 c2)))))

(defun is-bab-of-aba? (bab-candidate aba)
  (assert (= (length bab-candidate) 3))
  (when (not (or (scan "\\[" bab-candidate)
                 (scan "\\]" bab-candidate)))
    (let ((re (format nil "~A~A~A"
                      (aref aba 1)
                      (aref aba 0)
                      (aref aba 1))))
      (scan re bab-candidate))))

(defun has-abba-outside-parens? (ip-address)
  (let ((in-parens nil))
    (loop for c across ip-address
          for pos from 0
          when (char= #\[ c)
            do (setf in-parens t)
          when (char= #\] c)
            do (setf in-parens nil)
          when (and
                (< pos (- (length ip-address) 3))
                (proper-palindrome? (subseq ip-address pos (+ pos 4)))
                (not in-parens))
            do (return t))))

(defun has-abba-inside-parens? (ip-address)
  (let ((in-parens nil))
    (loop for c across ip-address
          for pos from 0
          when (char= #\[ c)
            do (setf in-parens t)
          when (char= #\] c)
            do (setf in-parens nil)
          when (and
                (< pos (- (length ip-address) 3))
                (proper-palindrome? (subseq ip-address pos (+ pos 4)))
                in-parens)
            do (return t))))

(defun support-tls? (ip-address)
  (and (has-abba-outside-parens? ip-address)
       (not (has-abba-inside-parens? ip-address))))

;; (support-tls? "abba[mnop]qrst") ;; yes
;; (support-tls? "abcd[bddb]xyyx") ;; no
;; (support-tls? "aaaa[qwer]tyui") ;; no
;; (support-tls? "ioxxoj[asdfgh]zxcvbn") ;; yes
;; (has-abba-outside-parens? "abcd[bdab]xyyx")
;; (support-tls? "abcd[bdab]xyyx") ;; yes

(defun aoc2016/day7/solution1 ()
  (length (remove-if-not #'support-tls? (read-ip-addresses-list))))

(defun aba-outside-parens (ip-address)
  (let ((in-parens nil))
    (loop for c across ip-address
          for pos from 0
          when (char= #\[ c)
            do (setf in-parens t)
          when (char= #\] c)
            do (setf in-parens nil)
          when (and
                (< pos (- (length ip-address) 2))
                (proper-palindrome-3? (subseq ip-address pos (+ pos 3)))
                (not in-parens))
            collect (subseq ip-address pos (+ pos 3)))))

(defun has-bab-inside-parens? (ip-address aba)
  (let ((in-parens nil))
    (loop for c across ip-address
          for pos from 0
          when (char= #\[ c)
            do (setf in-parens t)
          when (char= #\] c)
            do (setf in-parens nil)
          when (and
                in-parens
                (< pos (- (length ip-address) 2))
                (is-bab-of-aba? (subseq ip-address pos (+ pos 3))
                                aba))
            do (return t))))

(defun support-ssl? (ip-address)
  (loop for aba in (aba-outside-parens ip-address)
        when (has-bab-inside-parens? ip-address aba)
          do (return t)))

(defun aoc2016/day7/solution2 ()
  (length (remove-if-not #'support-ssl? (read-ip-addresses-list))))
