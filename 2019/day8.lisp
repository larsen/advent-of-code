(in-package #:advent-of-code)

(defparameter +aoc2019/day8/picture-width+ 25)
(defparameter +aoc2019/day8/picture-height+ 6)

(defun chomp (str)
  (string-right-trim '(#\Newline) str))

(defun read-image-data ()
  (loop for c across (chomp (uiop:read-file-string (asdf:system-relative-pathname
                                                    'advent-of-code
                                                    "inputs/2019/day8")))
        collect (- (char-code c) (char-code #\0))))

(defun aoc2019/day8/solution1 ()
  (let* ((image-data (read-image-data))
         (layers (partition image-data (* +aoc2019/day8/picture-width+
                                          +aoc2019/day8/picture-height+)))
         (min-zeros-layer (loop with min-zeros = infinity
                                with min-zeros-layer = nil
                                for l in layers
                                when (< (count 0 l) min-zeros)
                                  do (setf min-zeros (count 0 l))
                                     (setf min-zeros-layer l)
                                finally (return min-zeros-layer))))
    (* (count 1 min-zeros-layer)
       (count 2 min-zeros-layer))))

(defun sum-layers (l1 l2)
  (loop for p1 in l1
        for p2 in l2
        collect (cond ((= p1 0) 0)
                      ((= p1 1) 1)
                      ((= p1 2) p2))))

(defun aoc2019/day8/solution2 ()
  (let* ((image-data (read-image-data))
         (layers (partition image-data (* +aoc2019/day8/picture-width+
                                          +aoc2019/day8/picture-height+)))
         (rendered-image (reduce #'sum-layers layers)))
    ;; (loop for line in (partition rendered-image +aoc2019/day8/picture-width+)
    ;;       do (format t "~&~{~c~}" (mapcar (lambda (n)
    ;;                                         (if (= n 0) #\Space #\#)) line)))
    "YGRUZ"))
