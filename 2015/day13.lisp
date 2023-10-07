(in-package #:advent-of-code)

(defun read-happiness-rules ()
  (flet ((ensure-subhash (h key)
           (unless (gethash h key)
             (setf (gethash h key)
                   (make-hash-table :test #'eql)))))
    (let ((happiness-graph (make-hash-table :test #'eql))
          (re "(\\w+) would (gain|lose) (\\d+) happiness units by sitting next to (\\w+)."))
      (loop for line in (uiop:read-file-lines
                         (asdf:system-relative-pathname 'advent-of-code
                                                        "inputs/2015/day13"))
            do (register-groups-bind ((#'symbolicate subject)
                                      gain-lose
                                      (#'parse-integer increment)
                                      (#'symbolicate neighbour))
                   (re line)
                 (ensure-subhash subject happiness-graph)
                 (setf (gethash neighbour (gethash subject happiness-graph))
                       (if (string= gain-lose "gain")
                           increment
                           (* increment -1))))
            finally (return happiness-graph)))))

(defun happiness-for-placement (placement happiness-graph)
  (loop for idx from 0
        for prev = (if (= idx 0)
                       (first (last placement))
                       (nth (- idx 1) placement))
        for curr in placement
        for succ = (if (= idx (- (length placement) 1))
                       (first placement)
                       (nth (+ idx 1) placement))
        summing (+ (gethash prev (gethash curr happiness-graph))
               (gethash succ (gethash curr happiness-graph)))
          into total
        finally (return total)))

(defun aoc2015/day13/solution1 ()
  (let ((happiness-graph (read-happiness-rules))
        (happiness-levels '()))
    (map-permutations
     (lambda (permutation)
       (push (happiness-for-placement permutation happiness-graph)
             happiness-levels))
     (hash-table-keys happiness-graph))
    (apply #'max happiness-levels)))

(defun aoc2015/day13/solution2 ()
  (let ((happiness-graph (read-happiness-rules))
        (happiness-levels '())
        (tmp-happiness-graph))
    ;; Now adding myself to the structure
    (setf (gethash 'me happiness-graph) (make-hash-table :test #'eql))
    (loop for subject in (hash-table-keys happiness-graph)
          do (setf (gethash 'me (gethash subject happiness-graph)) 0)
             (setf (gethash subject (gethash 'me happiness-graph)) 0))
    (loop for p in (hash-table-keys happiness-graph)
          do (setf tmp-happiness-graph (copy-hash-table happiness-graph))
             (remhash p tmp-happiness-graph)
             (map-permutations
              (lambda (permutation)
                ; (print (cons p permutation))
                (push (happiness-for-placement (cons p permutation) happiness-graph)
                      happiness-levels))
              (hash-table-keys tmp-happiness-graph)))
    (loop for hl in happiness-levels
          maximize hl into max-hl
          finally (return max-hl))))
