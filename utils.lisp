(in-package #:advent-of-code)

(defun read-input-file (filename)
  (uiop:read-file-string
   (asdf:system-relative-pathname 'advent-of-code filename)))

(defun read-input-file-as-lines (filename)
  (uiop:read-file-lines
   (asdf:system-relative-pathname 'advent-of-code filename)))

(let ((last-returned-value))
  (defun flip-flop (value1 value2)
    (unless last-returned-value
      (setf last-returned-value value2))
    (setf last-returned-value
          (if (equal last-returned-value value1)
              value2
              value1)))
  (defun reset-flip-flop ()
    (setf last-returned-value nil)))

(defun remove-nth (n lst)
  (append (subseq lst 0 (- n 1))
          (subseq lst n (length lst))))

(defun replace-substr (str start end substr)
  "Returns a new a string based on STR, where the characters from
START to END have been replaced by SUBSTR."
  (coerce
   (loop with replaced-p = nil
         for idx from 0
         for c across str
         ;; Can't I do something better?
         if (and (>= idx start)
                 (< idx end)
                 (not replaced-p))
           do (setf replaced-p t)
           and append (coerce substr 'list)
         if (or (< idx start)
                (>= idx end))
           collect c)
   'string))

(defun replace-sublist (lst start end sublst)
  "Returns a new list based on LST where the elements from
START to END (excluded) have been replaced by SUBLST."
  (loop with replaced-p = nil
        for idx from 0
        for elem in lst
        ;; Can't I do something better?
        if (and (>= idx start)
                (< idx end)
                (not replaced-p))
          do (setf replaced-p t)
          and append sublst
        if (or (< idx start)
               (>= idx end))
          collect elem))

(defun digits (n)
  "Returns the list of digits of N (as numbers)"
  (loop for d across (format nil "~A" n)
        collect (digit-char-p d)))

(defun pad (lst len padding)
  "Returns the original LST, with as many PADDING elements in front
as necessary to reach length LEN"
  (if (>= (length lst) len)
      lst
      (pad (cons padding lst) len padding)))

(defun read-csv-line (filename)
  (mapcar #'parse-integer
          (split "," (uiop:read-file-string filename))))

(defun triangular (n)
  (* 1/2 n (+ n 1)))

(defparameter infinity most-positive-fixnum)

(defun sum (lst)
  (reduce #'+ lst))

(defun transpose (lst)
  (apply #'mapcar #'list lst))


;; Sequences

(defclass wrapped-sequence ()
  ((sequence :initarg :sequence :accessor seq)
   (index :initform -1 :accessor index)))

(defgeneric reset (s))
(defmethod reset ((s wrapped-sequence))
  (setf (index s) -1))

(defgeneric next (s))
(defmethod next ((s wrapped-sequence))
  (when (= (index s) (- (length (seq s)) 1))
    (reset s))
  (incf (index s))
  (nth (index s) (seq s)))

;; TODO this could be replaced by #'runs in Serapeum
(defun streaks (lst partitions acc)
  (cond ((and (null lst)
              (not (null acc)))
         (streaks lst (cons acc partitions) '()))
        ((null lst) partitions)
        ((and (car lst)
              (null acc)) (streaks (cdr lst) partitions (cons (car lst) acc)))
        ((and (car lst)
              (not (null acc))
              (eql (car acc)
                   (car lst))) (streaks (cdr lst) partitions (cons (car lst) acc)))
        ((and (car lst)
              (not (null acc))
              (not (eql (car acc)
                        (car lst))))
         (streaks (cdr lst) (cons acc partitions) (cons (car lst) '())))))

(defun extractions (lst len)
  "Returns a list of all the possible sequences of length LEN built using
elements from LST (with repetitions)."
  (let ((results '()))
    (labels ((%extractions (lst len acc)
               (if (= len (length acc))
                   (push acc results)
                   (loop for el in lst
                         do (%extractions lst len (cons el acc))))))
      (%extractions lst len '()))
    results))

(defun permutations (lst)
  "Returns a list of all the possible sequences of length LEN built using
elements from LST (with repetitions)."
  (let ((results '()))
    (labels ((%extractions (avail acc)
               (if (null avail)
                   (push acc results))
               (loop for el in avail
                     do (%extractions (remove el avail)
                                      (cons el acc)))))
      (%extractions lst '()))
    results))

(defun but-last (lst)
  (loop for l on lst
        while (rest l)
        collect (first l)))
