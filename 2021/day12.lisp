(in-package #:advent-of-code)

(defun read-caves-system ()
  (let ((caves (make-hash-table :test 'equalp)))
    (loop for line in (uiop:read-file-lines
                       (asdf:system-relative-pathname 'advent-of-code "inputs/2021/day12"))
          do (register-groups-bind (node-a node-b)
                 ("(\\w+)-(\\w+)" line)
               (push node-b (gethash node-a caves '()))
               (push node-a (gethash node-b caves '()))))
    caves))

(defun is-big-cave-p (cave)
  (upper-case-p (aref cave 0)))

(defun aoc2021/day12/solution1 ()
  (let ((caves (read-caves-system))
        (paths-counter 0))
    (labels ((walk (cave path)
               (if (string= cave "end")
                   (incf paths-counter)
                   (loop for dest in (gethash cave caves)
                         when (or (is-big-cave-p dest)
                                  (not (find dest path :test 'equalp)))
                           do (walk dest (cons cave path))))))
      (walk "start" '()))
    paths-counter))

(defun aoc2021/day12/solution1-iterative ()
  (let ((caves (read-caves-system))
        (paths-counter 0))
    (loop with open-paths = '(("start"))
          until (emptyp open-paths)
          for op = (pop open-paths)
          do (if (string= "end" (first op))
                 (progn
                   (incf paths-counter))
                 (loop for dest in (gethash (first op) caves)
                       when (or (is-big-cave-p dest)
                                (not (find dest op :test 'equalp)))
                         do (push (cons dest op) open-paths))))
    paths-counter))

(defun count-small-caves-visited-twice (path)
  (let ((visits (make-hash-table :test 'equalp)))
    (loop for cave in path
          when (not (is-big-cave-p cave))
            do (incf (gethash cave visits 0)))
    (count 2 (hash-table-values visits))))

(defun aoc2021/day12/solution2 ()
  (let ((caves (read-caves-system))
        (paths-counter 0))
    (labels ((walk (cave path)
               (if (string= cave "end")
                   (progn
                     (incf paths-counter))
                   (loop for dest in (gethash cave caves)
                         when (or (is-big-cave-p cave)
                                  (not (find cave path :test 'equalp))
                                  (and
                                   (not (string= "start" cave))
                                   (zerop (count-small-caves-visited-twice path))))
                           do (walk dest (cons cave path))))))
      (walk "start" '()))
    paths-counter))
