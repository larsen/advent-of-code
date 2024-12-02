(in-package #:advent-of-code)

(defun read-input-file (filename)
  (uiop:read-file-string
   (asdf:system-relative-pathname 'advent-of-code filename)))

(defun read-input-file-as-lines (filename)
  (uiop:read-file-lines
   (asdf:system-relative-pathname 'advent-of-code filename)))

(defun read-all-the-numbers (filename)
  (mapcar #'parse-integer
          (split "\\s+" (uiop:read-file-string
                         (asdf:system-relative-pathname
                          :advent-of-code filename)))))

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

(defun safesum (lst)
  (sum (remove-if #'null lst)))

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

(defun find-index (item lst &key (test 'eql))
  "Find the index of ITEM in the list LST, or return NIL if not found."
  (loop for element in lst
        until (funcall test element item)
        count t))

(defun unique (sequence &key (test 'eql))
  (let ((seen (make-hash-table :test test)))
    (loop for e in sequence
          do (incf (gethash e seen 0))
          finally (return (hash-table-keys seen)))))

;; FIXME: it does not work with conses
(defun pairs (lst)
  "Returns all possible pairs made with elements of LST."
  (loop for elem1 in lst
        append (loop for elem2 in (remove elem1 lst)
                     collect (cons elem1 elem2))))

(defun partition (lst n)
  (loop for offset from 0 below (length lst) by n
        collect (subseq lst offset (+ offset n))))

;; Dijkstra

(defun general-shortest-path (map &key start goal
                                    (valid-p (lambda (dest src map)
                                               (declare (ignore dest src map)) T))
                                    (directions '((-1 . 0) (0 . 1) (1 . 0) (0 . -1))))
  "Applies Dijkstra shortest path algorithm to a graph represented with the
2D array MAP. In the array, adjacent elements are intended as adjacent
nodes in the graph. START and GOAL are a cons of coordinates (row and
col) in the array. VALID-P is an optional function that
returns a boolean value, to determine if an edge from DEST to SRC is
valid. DIRECTIONS refer to the directions in the map the algorithm
should consider to find another node (defaults to orthogonal
directions)"
  (let* ((shape (array-dimensions map))
         (dists (make-array shape :initial-element infinity))
         (non-visited-nodes (loop for r from 0 below (first shape)
                                  append (loop for c from 0 below (second shape)
                                               collect (cons r c)))))
    (labels ((extract-min-dist-node ()
               (let ((res (loop with min-dist = infinity
                                with min-node = nil
                                for node in non-visited-nodes
                                when (<= (aref dists (car node) (cdr node))
                                         min-dist)
                                  do (setf min-node node
                                           min-dist (aref dists (car node) (cdr node)))
                                finally (return min-node))))
                 (setf non-visited-nodes (remove res non-visited-nodes))
                 res))

             (adjacents (node valid-p)
               (loop for dir in directions
                     for candidate = (cons (+ (car node) (car dir))
                                           (+ (cdr node) (cdr dir)))
                     when (and (array-in-bounds-p map
                                                  (car candidate)
                                                  (cdr candidate))
                               (funcall valid-p candidate node map))
                       collect candidate)))

      ;; Dijkstra
      (loop for r from 0 below (first shape)
            do (loop for c from 0 below (second shape)
                     when (equal start (cons r c))
                       do (setf (aref dists r c) 0)))
      (loop while non-visited-nodes
            do (loop with u = (extract-min-dist-node)
                     for v in (adjacents u valid-p)
                     do (setf (aref dists (car v) (cdr v))
                              (min (aref dists (car v) (cdr v))
                                   (+ 1 (aref dists (car u) (cdr u)))))))
      (loop with res = nil
            for r from 0 below (first shape)
            do (loop for c from 0 below (second shape)
                     when (equal goal (cons r c))
                       do (setf res (aref dists r c)))
            finally (return (values res dists))))))
