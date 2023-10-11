(in-package #:advent-of-code)

(defstruct valve
  (label)
  (flow-rate 0)
  (adjacent '()))

(defun read-pipes-system ()
  (let ((h (make-hash-table :test #'equal)))
    (flet ((my-simbolicate (str)
             (intern (string-upcase str) :advent-of-code)))
      (loop for l in (uiop:read-file-lines
                      (asdf:system-relative-pathname :advent-of-code "inputs/2022/day16"))
            do (register-groups-bind (label
                                      (#'parse-integer flow-rate)
                                      adjacent-raw)
                   ("Valve (\\w+) has flow rate=(\\d+); tunnels? leads? to valves? ([\\w, ]+)" l)
                 (setf (gethash (my-simbolicate label) h)
                       (make-valve :label (my-simbolicate label)
                                   :flow-rate flow-rate
                                   :adjacent (mapcar #'my-simbolicate (split ", " adjacent-raw)))))
            finally (return h)))))

(defun valve-pressure (valve g)
  (valve-flow-rate
   (gethash valve g
            (error (format t "Valve ~a not found!" valve)))))

(defun floyd (graph)
  "Applies Floyd's all-pairs shortest path algorithm to GRAPH, expressed as a hash-table"
  (let* ((nodes (alexandria:hash-table-keys graph))
         (distances (make-hash-table)))
    ;; init hash of distances
    ;; setting the distance to each node from itself
    (loop for n in nodes
          do (setf (gethash n distances)
                   (loop for m in nodes
                         collect (cons m (cond ((eql n m) 0)
                                               ((member m (valve-adjacent (gethash n graph))) 1)
                                               (t most-positive-fixnum))))))
    (loop for k in nodes
          do (loop for n in nodes
                   do (loop for m in nodes
                            do (setf (cdr (assoc m (gethash n distances)))
                                     (min (cdr (assoc m (gethash n distances)))
                                          (+ (cdr (assoc k (gethash n distances)))
                                             (cdr (assoc m (gethash k distances)))))))))
    distances))

(defun aoc2022/day16/solution1 ()
  (let* ((graph (read-pipes-system))
         (distances (floyd graph))
         (max-flow-rate 0)
         (positive-flow-rate-nodes
           (remove-if (lambda (n)
                        (zerop (valve-flow-rate (gethash n graph))))
                      (alexandria:hash-table-keys graph)))
         (l-positive-flow-rate-notes (length positive-flow-rate-nodes)))
    (labels ((dist (from to)
               (cdr (assoc to (gethash from distances))))
             (flow-rate-for-valve-permutation (perm time)
               (loop with dt = time
                     for idx from 0 below (length perm)
                     for start-node = (if (> idx 0)
                                          (nth (- idx 1) perm)
                                          'aa)
                     for dest-node = (nth idx perm)
                     for moving-cost = (dist start-node dest-node)
                     while (>= dt 0)
                     do (decf dt (1+ moving-cost)) ;; moving to the node and opening the valve
                     sum (* dt (valve-flow-rate (gethash dest-node graph)))
                       into total-flow-rate
                     finally (return total-flow-rate)))
             (check-permutation (perm)
               (let ((fr (flow-rate-for-valve-permutation perm 30)))
                 (if (> fr max-flow-rate)
                     (setf max-flow-rate (max max-flow-rate fr))))))
      (alexandria:map-permutations #'check-permutation positive-flow-rate-nodes
                                   :length (alexandria:clamp (ceiling 30 4)
                                                             0 l-positive-flow-rate-notes))
      max-flow-rate)))

;; SLOW!
(defun aoc2022/day16/solution2-permutations ()
  (let* ((graph (read-pipes-system))
         (distances (floyd graph))
         (max-flow-rate 0)
         (positive-flow-rate-nodes
           (remove-if (lambda (n)
                        (zerop (valve-flow-rate (gethash n graph))))
                      (alexandria:hash-table-keys graph)))
         (l-positive-flow-rate-notes (length positive-flow-rate-nodes)))
    (labels ((dist (from to)
               (cdr (assoc to (gethash from distances))))
             (flow-rate-for-valve-permutation (perm time)
               (loop with dt = time
                     for idx from 0 below (length perm)
                     for start-node = (if (> idx 0)
                                          (nth (- idx 1) perm)
                                          'aa)
                     for dest-node = (nth idx perm)
                     for moving-cost = (dist start-node dest-node)
                     while (>= dt 0)
                     do (decf dt (1+ moving-cost)) ;; moving to the node and opening the valve
                     sum (* dt (valve-flow-rate (gethash dest-node graph)))
                       into total-flow-rate
                     finally (return total-flow-rate))))
      (print l-positive-flow-rate-notes)

      ;; The problem dataset implies iterating over the permutations
      ;; of 16 elements: too much. The real solution must examine less cases
      (alexandria:map-permutations
       (lambda (perm)
         (loop for n from (- (floor l-positive-flow-rate-notes 2) 1)
                 to (+ (floor l-positive-flow-rate-notes 2) 0)
               for v1 = (subseq perm 0 n)
               for v2 = (subseq perm n)
               for fr1 = (flow-rate-for-valve-permutation v1 26)
               for fr2 = (flow-rate-for-valve-permutation v2 26)
               when (> (+ fr1 fr2) max-flow-rate)
                 do (progn
                      (print v1) (print v2)
                      (setf max-flow-rate (max max-flow-rate (+ fr1 fr2)))
                      (print max-flow-rate))))
       positive-flow-rate-nodes)
      max-flow-rate)))

(defun aoc2022/day16/solution2 ()
  )



;; (ql:quickload :s-graphviz)
;; (s-graphviz:render-graph "/tmp/16.png" (generate-graph-from-input))
(defun generate-graph-from-input ()
  (let ((g (read-pipes-system)))
    `(:digraph "volcano"
               (= :rankdir "LR")
               (:node (:style :filled))
               ;; nodes
               ,@(loop for node being the hash-keys of g
                       collect `(,node (:label ,(format nil "~a~%✕~a"
                                                        node
                                                        (valve-flow-rate (gethash node g))))
                                       (:shape ,(if (eql node 'AA)
                                                    "doublecircle"
                                                    "circle"))
                                       (:fillcolor ,(if (> (valve-flow-rate (gethash node g)) 0)
                                                        "#aaaaff"
                                                        "#FFFFFF"))))
               ;; arcs
               ,@(loop for node being the hash-keys of g
                       append (mapcar (lambda (d)
                                        `(:-> nil ,node ,d))
                                      (valve-adjacent (gethash node g)))))))

(defun generate-reduced-graph-from-input ()
  (let* ((g (read-pipes-system))
         (distances (floyd g)))
    `(:digraph "volcano-floyd"
               (= :rankdir "LR")
               (:node (:style :filled))
               ;; nodes
               ,@(loop for node being the hash-keys of distances
                       when (or (> (valve-flow-rate (gethash node g)) 0)
                                (equal 'AA node))
                         collect `(,node (:label ,(format nil "~a~%✕~a"
                                                          node
                                                          (valve-flow-rate (gethash node g))))
                                         (:shape ,(if (eql node 'AA)
                                                      "doublecircle"
                                                      "circle"))
                                         (:fillcolor ,(if (> (valve-flow-rate (gethash node g)) 0)
                                                          "#aaaaff"
                                                          "#FFFFFF"))))
               ;; arcs
               ,@(loop for node being the hash-keys of distances
                       when (or (> (valve-flow-rate (gethash node g)) 0)
                                (equal 'AA node))
                         append (mapcar (lambda (d)
                                          `(:-> nil ,node ,(car d)))
                                        (gethash node distances)))
               )))

(defun current-flow (open-valves g)
  (loop for idx from 1 to 30
        for v across open-valves
        when v
          sum (* idx (valve-flow-rate (gethash v g)))))

(defun already-open-p (valve open-valves)
  (member valve (coerce open-valves 'list)))

(defun show-open-valves (open-valves graph)
  (loop for idx from 1
        for v across open-valves
        when v collect (list v idx (valve-flow-rate (gethash v graph)))))

(defun closed-valves (open-valves g)
  (loop for v being the hash-keys of g
        when (not (already-open-p v open-valves))
          collect v))

(defun open-valve (valve remaining-time open-valves)
  (let ((c (alexandria:copy-array open-valves)))
    (setf (aref c (- remaining-time 1)) valve)
    c))

;; Too slow!
;; To test: (walk-and-open-valves 30)
(defun walk-and-open-valves (time-before-eruption)
  (let ((graph (read-pipes-system))
        (max-flow 0))
    (labels ((walk (remaining-time node open-valves)
               (if (or (= 1 remaining-time)
                       (loop for v in (closed-valves open-valves graph)
                             always (= 0 (valve-flow-rate (gethash v graph)))))
                   (when (> (current-flow open-valves graph) max-flow)
                       (setf max-flow (current-flow open-valves graph))
                       (format t "new max flow ~a (valves: ~a) ~%"
                               max-flow
                               (show-open-valves open-valves graph)))
                   (progn
                     ;; if not open && flow-rate > 0, try opening the valve
                     (if (and (not (already-open-p node open-valves))
                              (> (valve-flow-rate (gethash node graph)) 0))
                         (walk (- remaining-time 1)
                               node
                               (open-valve node (- remaining-time 1) open-valves)))
                     ;; try ajacent nodes
                     (loop for adj in (valve-adjacent (gethash node graph))
                           do (walk (- remaining-time 1)
                                    adj
                                    open-valves))))))
      (walk time-before-eruption 'AA
            (make-array (+ time-before-eruption 1)
                        :initial-element nil))
      max-flow)))
