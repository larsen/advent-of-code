(in-package #:advent-of-code)

(defun read-string-literals-as-bytes ()
  (with-open-file (stream (asdf:system-relative-pathname 'advent-of-code
                                                       "inputs/2015/day8")
                        :direction :input
                        :element-type 'unsigned-byte)
  (loop with counter = 0
        for c = (read-byte stream nil)
        while c collect c)))

(defun count-bytes-in-strings (bytes)
  (count-if-not (lambda (c) (= c 10)) bytes))

(defun length-unescaped (bytes)
  (loop with char-counter = 0
        with pos = 0
        while (< pos (length bytes))
        do ;; Simple escape sequence
           (if (= 92 (nth pos bytes))
               (if (= 120 (nth (+ pos 1) bytes))
                   ;; \x.. escape sequence
                   (incf pos 4)
                   (incf pos 2))
               (incf pos))
           (incf char-counter)
        finally (return char-counter)))

(defun unescaped-sequences-length (bytes)
  (reduce #'+ (mapcar (lambda (s)
                        (if s
                          (length-unescaped
                           (subseq s 1 (- (length s) 1)))
                          0))
                      (split-sequence 10 bytes))))

(defun aoc2015/day8/solution1 ()
  (let* ((literals-as-bytes (read-string-literals-as-bytes)))
    (- (count-bytes-in-strings literals-as-bytes)
       (unescaped-sequences-length literals-as-bytes))))

(defun length-escaped (bytes)
  (loop with char-counter = 0
        with pos = 0
        while (<= pos (- (length bytes) 1))
        do (case (nth pos bytes)
             ;; " needs two chars to be encoded
             (34 (incf char-counter 2))
             ;; \ needs two chars to be encoded
             (92 (incf char-counter 2))
             (t  (incf char-counter)))
           (incf pos)
        ;; at the end, add two characters to represent the quotes
        finally (return (+ 2 char-counter))))

(defun escaped-sequences-length (bytes)
    (reduce #'+ (mapcar (lambda (s)
                        (if s (length-escaped s) 0))
                      (split-sequence 10 bytes))))

(defun aoc2015/day8/solution2 ()
  (let* ((literals-as-bytes (read-string-literals-as-bytes)))
    (- (escaped-sequences-length literals-as-bytes)
       (count-bytes-in-strings literals-as-bytes))))
