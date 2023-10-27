(in-package #:advent-of-code)

(defun read-steps-instructions ()
  (let ((g (make-hash-table :test 'equal)))
    (loop for l in (uiop:read-file-lines
                    (asdf:system-relative-pathname
                     :advent-of-code "inputs/2018/day7"))
          do (register-groups-bind (start end)
                      ("Step (\\w) must be finished before step (\\w) can begin." l)
               (push end (gethash start g '())))
          finally (return g))))

(defun %print-g (g)
  (loop for k being the hash-keys of g
        do (format t "~&~A: ~{~A, ~}" k (gethash k g))))

;; Topological sort

(defun has-entering-edges-p (node g)
  (loop for v being the hash-values of g
          thereis (find node v :test 'string=)))

(defun find-ordered-available-nodes (g)
  (sort
   (loop for v being the hash-keys of g
         when (not (has-entering-edges-p v g))
           collect v)
   #'string<))

(defun topological-sort (g)
  (let* ((available (find-ordered-available-nodes g))
         (sorted-nodes '()))
    ;; (%print-g g)
    ;; (format t "Available at start: ~a" available)
    (loop
      for node = (pop available)
      do (push node sorted-nodes)

         (loop with adjs = (gethash node g)
               for adj in adjs
               do (setf (gethash node g)
                        (remove adj (gethash node g)))
                  (if (not (has-entering-edges-p adj g))
                      (push adj available)))

         ;; we choose nodes alphabetically in case of multiple sortings
         (setf available (sort (unique available :test 'equal) #'string<))
         ;; (format t "~&current available: ~A sorted: ~A, available: ~A"
         ;;         node (reverse sorted-nodes) available)
         ;; (%print-g g)
      while available
      finally (return (reverse sorted-nodes)))))

(defun aoc2018/day7/solution1 ()
  (apply #'concatenate 'string (topological-sort (read-steps-instructions))))

(defstruct (construction-worker
            (:print-function (lambda (w s d)
                               (declare (ignore d))
                               (format s "<~a: ~a ~a ETA: ~a>"
                                       (construction-worker-id w)
                                       (construction-worker-busy w)
                                       (construction-worker-task w)
                                       (construction-worker-time-to-completion w)))))
  (id)
  (task)
  (busy nil)
  (time-to-completion 0 :type integer))


(defun task-length (task)
  (+ 60 1 (- (char-code (char task 0))
             (char-code #\A))))

(defun perform-tasks-in-topological-order (g)
  (loop with elapsed-time = 0
        with available = (find-ordered-available-nodes g)
        with done = '()
        with workers = (loop for counter from 0 repeat 5 collect (make-construction-worker :id counter))
        do
           ;; Display the status of the system
           ;; (format t "~&T ~a ~%Available tasks: ~a Done: ~a~%" elapsed-time available done)
           ;; (format t "~{~a ~}~%" workers)

           ;; Update workers that may have finished some work
           (loop for w in workers
                 when (and (construction-worker-busy w)
                           (= 0 (construction-worker-time-to-completion w)))
                   ;; A task is finished!
                   do
                      ;; Update the graph
                      (loop with adjs = (gethash (construction-worker-task w) g)
                            for adj in adjs
                            do (setf (gethash (construction-worker-task w) g)
                                     (remove adj (gethash (construction-worker-task w) g)))
                               (if (and (not (has-entering-edges-p adj g))
                                        (not (find adj available)))
                                   (progn
                                     ;; (format t "adding ~a to ~a~%" adj available)
                                     (push adj available))))
                      ;; Update the worker
                      ;; (format t "Task ~a completed!~%" (construction-worker-task w))
                      (push (construction-worker-task w) done)
                      (setf (construction-worker-busy w) nil))

           ;; Reorder available tasks
           (setf available (sort (unique available :test 'equal) #'string<))

           ;; Assign available and unassigned tasks to workers that are not busy
           (loop for w in workers while available
                 when (not (construction-worker-busy w))
                   do (setf (construction-worker-busy w) T)
                      (setf (construction-worker-task w)
                            (pop available))

                      (setf (construction-worker-time-to-completion w)
                            (task-length (construction-worker-task w)))
                      ;; (format t "Assigned task ~a to ~a~%" (construction-worker-task w) w)
                 )

           ;; Advance time
        when (or available
                 (loop for w in workers
                         thereis (construction-worker-busy w)))
          do (incf elapsed-time)
             (loop for w in workers
                   when (construction-worker-busy w)
                     do (decf (construction-worker-time-to-completion w)))

        while (or available
                  (loop for w in workers
                          thereis (construction-worker-busy w)))

        finally (return elapsed-time)))

(defun aoc2018/day7/solution2 ()
  (perform-tasks-in-topological-order (read-steps-instructions)))



;; (ql:quickload :s-graphviz)
;; (s-graphviz:render-graph "/tmp/graph.png" (generate-graphviz (read-steps-instructions)))
(defun generate-graphviz (g &key (visited nil))
  "Returns a structure that can be passed to #s-graphviz:render-graph.
Assumes the graph is represented by a hash-table where keys are the
starting node of edges, and values are lists of ends of the edges."
  `(:digraph "steps"
             (= :rankdir "LR")
             (:node (:style :filled))
             ;; nodes
             ,@(loop for node being the hash-keys of g
                     collect `(,node (:label ,(format nil "~a" node))
                                     (:fillcolor ,(if (find node visited)
                                                      "#aaaaff"
                                                      "#FFFFFF"))))
             ;; arcs
             ,@(loop for node being the hash-keys of g
                     append (mapcar (lambda (d)
                                      `(:-> nil ,node ,d))
                                    (gethash node g)))))
