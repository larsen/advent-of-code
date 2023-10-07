(in-package #:advent-of-code)

(defun read-distances ()
  (let ((adjacency-graph (make-hash-table :test #'equal)))
    (loop for line in (uiop:read-file-lines
                       (asdf:system-relative-pathname 'advent-of-code
                                                      "inputs/2015/day9"))
          do (register-groups-bind (city1 city2 (#'parse-integer distance))
                 ("(\\w+) to (\\w+) = (\\d+)" line)
               (let ((city1-distances (gethash city1 adjacency-graph
                                               (make-hash-table :test #'equal)))
                     (city2-distances (gethash city2 adjacency-graph
                                               (make-hash-table :test #'equal))))
                 (setf (gethash city2 city1-distances) distance)
                 (setf (gethash city1 city2-distances) distance)
                 ;; Is there a better idiom?
                 (setf (gethash city1 adjacency-graph) city1-distances)
                 (setf (gethash city2 adjacency-graph) city2-distances)))
          finally (return adjacency-graph))))

(defun distance (city1 city2 adjacency-graph)
  (gethash city2 (gethash city1 adjacency-graph)))

(defun path-length (path adjacency-graph)
  (cond
    ((= 0 (length path)) (error "Malformed path: ~a" path))
    ((= 1 (length path)) 0)
    (t (+ (distance (car path) (cadr path) adjacency-graph)
          (path-length (cdr path) adjacency-graph)))))

(defun search-shortest-path (graph)
    (let ((best-path-length 1000000))
      (defun build-path (path graph)
        (if (= (length path) (length (hash-table-keys graph)))
            (if (< (path-length (reverse path) graph)
                   best-path-length)
                (setf best-path-length (path-length (reverse path) graph))))
        (when (gethash (car path) graph)
          (dolist (k (hash-table-keys (gethash (car path) graph)))
            (if (not (member k path :test #'equal))
                (build-path (cons k path) graph)))))
      (loop for starting-city being the hash-keys of graph
            do (build-path (list starting-city) graph))
      best-path-length))

(defun aoc2015/day9/solution1 ()
  (search-shortest-path (read-distances)))

(defun search-longest-path (graph)
    (let ((best-path-length 0))
      (defun build-path (path graph)
        (if (= (length path) (length (hash-table-keys graph)))
            (if (> (path-length (reverse path) graph)
                   best-path-length)
                (setf best-path-length (path-length (reverse path) graph))))
        (when (gethash (car path) graph)
          (dolist (k (hash-table-keys (gethash (car path) graph)))
            (if (not (member k path :test #'equal))
                (build-path (cons k path) graph)))))
      (loop for starting-city being the hash-keys of graph
            do (build-path (list starting-city) graph))
      best-path-length))

(defun aoc2015/day9/solution2 ()
  (search-longest-path (read-distances)))
