(in-package #:advent-of-code)

(defun read-droplet-cubes ()
  (mapcar (lambda (str)
            (apply #'v! (mapcar #'parse-integer (split "," str))))
          (uiop:read-file-lines
           (asdf:system-relative-pathname :advent-of-code "inputs/2022/day18"))))

(defparameter *directions-alist*
  `((:up    . ,(v! 0 0 1))
    (:down  . ,(v! 0 0 -1))
    (:left  . ,(v! -1 0 0))
    (:right . ,(v! 1 0 0))
    (:forward . ,(v! 0 1 0))
    (:backward . ,(v! 0 -1 0))))

(defun count-exposed-faces (droplet droplet-lst)
  ;; A droplet is a vector
  (loop for dir in (mapcar #'first *directions-alist*)
        count (not (member (v:+ droplet (cdr (assoc dir *directions-alist*)))
                           droplet-lst :test 'equalp))))

(defun aoc2022/day18/solution1 ()
  (let ((droplets (read-droplet-cubes)))
    (loop for droplet in droplets
          sum (count-exposed-faces droplet droplets))))

(defun aoc2022/day18/solution2 ()
  (let ((droplets (read-droplet-cubes)))
    (loop for droplet in droplets
          for exposed-faces = (count-exposed-faces droplet droplets)
          when (> exposed-faces 0)
            sum (count-exposed-faces droplet droplets))))
