(in-package #:advent-of-code)

(defun read-chiton-map ()
  (let* ((raw-lines (uiop:read-file-lines
                     (asdf:system-relative-pathname
                      'advent-of-code "inputs/2021/day15-test")))
         (width (length (first raw-lines)))
         (height (length raw-lines)))
    (magicl:from-list
     (mapcar #'parse-integer
             (loop for line in raw-lines
                   append (split "" line)))
     (list height width))))

(defun chiton-neighbours (row col grid)
  (loop for (dx dy) in '(        (+0 -1)
                         (-1 +0)         (+1 +0)
                                (+0 +1))
        when (handler-case
                 (magicl:tref grid (+ row dy) (+ col dx))
               (condition () nil))
          collect (list (+ row dy) (+ col dx))))

(defun dijkstra (grid start end)
  (let ((distances (magicl:copy-tensor grid))
        (visited '()))
    (labels ((visited (node)
               (find node visited :test 'equalp)))

      (magicl:map! (lambda (el)
                     (declare (ignore el))
                     10000)
                   distances)

      (push start visited)
      (setf (magicl:tref distances (first start) (second start)) 0)

      (loop with (c-row c-col) = start
            do (loop for (n-row n-col) in (chiton-neighbours c-row c-col grid)
                     when (not (visited (list n-row n-col)))
                       do (setf (magicl:tref distances n-row n-col)
                                (+ (magicl:tref grid n-row n-col)
                                   (magicl:tref distances c-row c-col)))

                          (setf (magicl:tref visited c-row c-col) 1)
                          (push (list c-row c-col) visited))
               ;; Select the new visited that has the lowest distance

            until (equalp (list c-row c-col) end))

      (print grid)
      (print distances))))

(defun aoc2021/day15/solution1 ()
  (let* ((chiton-map (read-chiton-map))
         (map-shape (magicl:shape chiton-map))
         (start '(0 0))
         (end (list (- (first map-shape) 1)
                    (- (second map-shape) 1))))
    (dijkstra (read-chiton-map) start end)))

(defun aoc2021/day15/solution2 ()
  )
