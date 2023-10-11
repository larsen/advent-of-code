(in-package #:advent-of-code)

(defun read-height-map ()
  (let ((map-raw (uiop:read-file-lines (asdf:system-relative-pathname
                                        :advent-of-code "inputs/2022/day12"))))
    (make-array (list (length map-raw)
                      (length (car map-raw)))
                :initial-contents (mapcar (lambda (s) (coerce s 'list))
                                          map-raw))))

(defun elevation (node map)
  (let ((c (aref map (car node) (cdr node))))
    (case c
      (#\S (char-code #\a))
      (#\E (char-code #\z))
      (t   (char-code c)))))

(defun shortest-path (map &key (start #\S) (goal #\E) (valid-p (lambda (elev-dest elev-src)
                                                                 (<= (- elev-dest elev-src) 1))))
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
             (loop for dir in '((-1 . 0) (0 . 1) (1 . 0) (0 . -1))
                   for candidate = (cons (+ (car node) (car dir))
                                         (+ (cdr node) (cdr dir)))
                   when (and (array-in-bounds-p map
                                                (car candidate)
                                                (cdr candidate))
                             (funcall valid-p
                                      (elevation candidate map)
                                      (elevation node map)))
                     collect candidate)))

      ;; Dijkstra
      (loop for r from 0 below (first shape)
            do (loop for c from 0 below (second shape)
                     when (char= start (aref map r c))
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
                     when (char= goal (aref map r c))
                       do (setf res (aref dists r c)))
            finally (return (values res dists))))))

(defun aoc2022/day12/solution1 ()
  (let ((map (read-height-map)))
    (shortest-path map :start #\S
                       :goal #\E
                       :valid-p (lambda (elev-dest elev-src)
                                  (<= (- elev-dest elev-src) 1)))))

(defun aoc2022/day12/solution2 ()
  (let* ((map (read-height-map))
         (shape (array-dimensions map))
         (dists (nth-value 1 (shortest-path map :start #\E
                                           :goal #\S
                                           :valid-p (lambda (elev-dest elev-src)
                                                      (>= (- elev-dest elev-src) -1))))))
    (loop with m = infinity
          for r from 0 below (first shape)
          do (loop for c from 0 below (second shape)
                   when (char= #\a (aref map r c))
                     do (if (< (aref dists r c) m )
                            (setf m (aref dists r c))) )
          finally (return m))))
