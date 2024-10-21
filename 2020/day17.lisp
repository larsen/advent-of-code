(in-package #:advent-of-code)

(defun sum-v3 (v1 v2)
  "Sum two V3 values (v1 and v2)."
  (vector (+ (aref v1 0)
             (aref v2 0))
          (+ (aref v1 1)
             (aref v2 1))
          (+ (aref v1 2)
             (aref v2 2))))

(defun sum-v4 (v1 v2)
  "Sum two V4 values (v1 and v2)."
  (vector (+ (aref v1 0)
             (aref v2 0))
          (+ (aref v1 1)
             (aref v2 1))
          (+ (aref v1 2)
             (aref v2 2))
          (+ (aref v1 3)
             (aref v2 3))))

(defun read-initial-cubes ()
  (let ((file-content (uiop:read-file-lines
                       (asdf:system-relative-pathname 'advent-of-code "inputs/2020/day17")))
        (cubes-configuration (make-hash-table :test #'equalp)))
    (loop for l in file-content
          for y from 0
          do (loop for c across l
                   for x from 0
                   do (setf (gethash (vector x y 0) cubes-configuration)
                            (if (char= c #\#)
                                :active
                                :inactive))))
    cubes-configuration))

(defparameter *day17/directions*
  '(;; z-1
    #(-1 -1 -1) #(+0 -1 -1) #(+1 -1 -1)
    #(-1 +0 -1) #(+0 +0 -1) #(+1 +0 -1)
    #(-1 +1 -1) #(+0 +1 -1) #(+1 +1 -1)

    ;; z
    #(-1 -1 +0) #(+0 -1 +0) #(+1 -1 +0)
    #(-1 +0 +0)             #(+1 +0 +0)
    #(-1 +1 +0) #(+0 +1 +0) #(+1 +1 +0)

    ;; z+1
    #(-1 -1 +1) #(+0 -1 +1) #(+1 -1 +1)
    #(-1 +0 +1) #(+0 +0 +1) #(+1 +0 +1)
    #(-1 +1 +1) #(+0 +1 +1) #(+1 +1 +1)))

(defun count-active-neighbours (cube cubes-configuration)
  (count :active
         (loop for dir in *day17/directions*
               collect (gethash (sum-v3 cube dir) cubes-configuration :inactive))))

(defun cycle (cubes-configuration)
  (let ((directions *day17/directions*)
        (new-cube-configuration (make-hash-table :test #'equalp)))
    (loop for cube in (hash-table-keys cubes-configuration)
          do (loop for dir in directions
                   do (let ((active-neighbours
                              (count-active-neighbours (sum-v3 cube dir)
                                                       cubes-configuration)))
                        (if (eql (gethash (sum-v3 cube dir) cubes-configuration :inactive)
                                 :inactive)
                            ;; cube currently inactive
                            (if (= active-neighbours 3)
                                (setf (gethash (sum-v3 cube dir) new-cube-configuration)
                                      :active))
                            ;; cube currently active
                            (setf (gethash (sum-v3 cube dir) new-cube-configuration
                                           )
                                  (if (not (or (= active-neighbours 2)
                                               (= active-neighbours 3)))
                                               :inactive
                                               :active))))))
    (values
     new-cube-configuration
     (count :active (hash-table-values new-cube-configuration)))))

(defun aoc2020/day17/solution1 ()
  (let ((cubes-configuration (read-initial-cubes)))
    (loop repeat 6
          do (setf cubes-configuration (cycle cubes-configuration))
          finally (return (count :active (hash-table-values cubes-configuration))))))

(defun read-initial-cubes-v4 ()
  (let ((file-content (uiop:read-file-lines
                       (asdf:system-relative-pathname
                        'advent-of-code "inputs/2020/day17")))
        (cubes-configuration (make-hash-table :test #'equalp)))
    (loop for l in file-content
          for y from 0
          do (loop for c across l
                   for x from 0
                   do (setf (gethash (vector x y 0 0) cubes-configuration)
                            (if (char= c #\#)
                                :active
                                :inactive))))
    cubes-configuration))

(defparameter *day17/directions-v4*
  '(
    ;; z=-1, w=-1
    #(-1 -1 -1 -1) #(+0 -1 -1 -1) #(+1 -1 -1 -1)
    #(-1 +0 -1 -1) #(+0 +0 -1 -1) #(+1 +0 -1 -1)
    #(-1 +1 -1 -1) #(+0 +1 -1 -1) #(+1 +1 -1 -1)

    ;; z, w=-1
    #(-1 -1 +0 -1) #(+0 -1 +0 -1) #(+1 -1 +0 -1)
    #(-1 +0 +0 -1) #(+0 +0 +0 -1) #(+1 +0 +0 -1)
    #(-1 +1 +0 -1) #(+0 +1 +0 -1) #(+1 +1 +0 -1)

    ;; z+1, w=-1
    #(-1 -1 +1 -1) #(+0 -1 +1 -1) #(+1 -1 +1 -1)
    #(-1 +0 +1 -1) #(+0 +0 +1 -1) #(+1 +0 +1 -1)
    #(-1 +1 +1 -1) #(+0 +1 +1 -1) #(+1 +1 +1 -1)

    ;; z=-1, w=0
    #(-1 -1 -1 0) #(+0 -1 -1 0) #(+1 -1 -1 0)
    #(-1 +0 -1 0) #(+0 +0 -1 0) #(+1 +0 -1 0)
    #(-1 +1 -1 0) #(+0 +1 -1 0) #(+1 +1 -1 0)

    ;; z, w=0
    #(-1 -1 +0 0) #(+0 -1 +0 0) #(+1 -1 +0 0)
    #(-1 +0 +0 0)               #(+1 +0 +0 0)
    #(-1 +1 +0 0) #(+0 +1 +0 0) #(+1 +1 +0 0)

    ;; z+1, w=0
    #(-1 -1 +1 0) #(+0 -1 +1 0) #(+1 -1 +1 0)
    #(-1 +0 +1 0) #(+0 +0 +1 0) #(+1 +0 +1 0)
    #(-1 +1 +1 0) #(+0 +1 +1 0) #(+1 +1 +1 0)

    ;; z=-1, w=1
    #(-1 -1 -1 +1) #(+0 -1 -1 +1) #(+1 -1 -1 +1)
    #(-1 +0 -1 +1) #(+0 +0 -1 +1) #(+1 +0 -1 +1)
    #(-1 +1 -1 +1) #(+0 +1 -1 +1) #(+1 +1 -1 +1)

    ;; z, w=1
    #(-1 -1 +0 +1) #(+0 -1 +0 +1) #(+1 -1 +0 +1)
    #(-1 +0 +0 +1) #(+0 +0 +0 +1) #(+1 +0 +0 +1)
    #(-1 +1 +0 +1) #(+0 +1 +0 +1) #(+1 +1 +0 +1)

    ;; z+1, w=1
    #(-1 -1 +1 +1) #(+0 -1 +1 +1) #(+1 -1 +1 +1)
    #(-1 +0 +1 +1) #(+0 +0 +1 +1) #(+1 +0 +1 +1)
    #(-1 +1 +1 +1) #(+0 +1 +1 +1) #(+1 +1 +1 +1)

    ))

(defun count-active-neighbours-v4 (cube cubes-configuration)
  (count :active
         (loop for dir in *day17/directions-v4*
               collect (gethash (sum-v4 cube dir) cubes-configuration :inactive))))

(defun cycle-v4 (cubes-configuration)
  (let ((directions *day17/directions-v4*)
        (new-cube-configuration (make-hash-table :test #'equalp)))
    (loop for cube in (hash-table-keys cubes-configuration)
          do (loop for dir in directions
                   do (let ((active-neighbours
                              (count-active-neighbours-v4 (sum-v4 cube dir)
                                                       cubes-configuration)))
                        (if (eql (gethash (sum-v4 cube dir) cubes-configuration :inactive)
                                 :inactive)
                            ;; cube currently inactive
                            (if (= active-neighbours 3)
                                (setf (gethash (sum-v4 cube dir) new-cube-configuration)
                                      :active))
                            ;; cube currently active
                            (setf (gethash (sum-v4 cube dir) new-cube-configuration
                                           )
                                  (if (not (or (= active-neighbours 2)
                                               (= active-neighbours 3)))
                                               :inactive
                                               :active))))))
    (values
     new-cube-configuration
     (count :active (hash-table-values new-cube-configuration)))))

(defun aoc2020/day17/solution2 ()
  (let ((cubes-configuration (read-initial-cubes-v4)))
    (loop repeat 6
          do (setf cubes-configuration (cycle-v4 cubes-configuration))
          finally (return (count :active (hash-table-values cubes-configuration))))))
