(in-package #:advent-of-code)

(defstruct my-file
  (name)
  (size))

(defstruct dir
  (name)
  (parent nil)
  (files '())
  (directories '()))

(defmethod print-object ((d dir) stream)
  (format stream "~a" (dir-name d)))

(defmethod size ((d dir))
  (+ (sum (mapcar #'my-file-size (dir-files d)))
     (sum (mapcar #'size (dir-directories d)))))

(defmethod find-dir ((d dir) name)
  (loop for sd in (dir-directories d)
        when (string= (dir-name sd) name)
          return sd))

(defun read-terminal-output ()
  (loop with root-dir = (make-dir :name "/")
        with current-dir = root-dir

        for line in (uiop:read-file-lines
                     (asdf:system-relative-pathname :advent-of-code "inputs/2022/day7"))

        do (register-groups-bind (token1 token2 token3)
               ("([^\\s]+)\\s([^\\s]+)\\s*([^\\s]*)" line)
             (cond
               ;; $ cd ..
               ((and (string= token2 "cd")
                     (string= token3 ".."))
                (setf current-dir (dir-parent current-dir)))

               ;; $ cd /
               ((and (string= token2 "cd")
                     (string= token3 "/"))
                (setf current-dir root-dir))

               ;; $  cd <dir>
               ((string= token2 "cd")
                (setf current-dir (find-dir current-dir token3)))

               ;; dir <nome>
               ((string= token1 "dir")
                (push (make-dir :name token2
                                :parent current-dir)
                      (dir-directories current-dir)))

               ;; 123... <name>
               ((scan "\\d+" token1)
                (push (make-my-file :name token2
                                 :size (parse-integer token1))
                      (dir-files current-dir)))))

          finally (return root-dir)))

(defun walk-tree (dir)
  (loop for d in (dir-directories dir)
        collect d
        append (walk-tree d)))

(defun aoc2022/day7/solution1 ()
  (let ((filesystem (read-terminal-output)))
    (loop for d in (walk-tree filesystem)
          when (<= (size d) 100000)
            sum (size d))))

(defparameter +total-space+ 70000000)
(defparameter +required-space+ 30000000)

(defun aoc2022/day7/solution2 ()
  (let* ((filesystem (read-terminal-output))
         (space-occupied (size filesystem)))
    (loop for d in (walk-tree filesystem)
          when (>= (size d) (- +required-space+
                               (- +total-space+ space-occupied)))
            minimizing (size d))))
