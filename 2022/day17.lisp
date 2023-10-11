(in-package #:advent-of-code)

(defun read-jet-pattern ()
  (coerce (uiop:stripln (uiop:read-file-string
                         (asdf:system-relative-pathname
                          :advent-of-code "inputs/2022/day17-test")))
          'list))

(defparameter *pieces* '((#c(0 0) #c(1 0) #c(2 0) #c(3 0))

                         (        #c(1 0)
                          #c(0 1) #c(1 1) #c(2 1)
                                  #c(1 2))

                         (                #c(2 0)
                                          #c(2 1)
                          #c(0 2) #c(1 2) #c(2 2))

                         (#c(0 0)
                          #c(0 1)
                          #c(0 2)
                          #c(0 3))

                         (#c(0 0) #c(1 0)
                          #c(0 1) #c(1 1))))

(defclass vertical-chamber ()
  ((chamber :initform (make-array '(7 7)) :accessor chamber)
   (ground-level :initform 0 :accessor ground-level)))

(defgeneric reset (chamber))
(defmethod reset ((chamber vertical-chamber))
  (setf (chamber chamber) (make-array '(7 7))))

(defmethod print-object ((chamber vertical-chamber) stream)
  (let* ((chamber (chamber chamber))
         (dimensions (array-dimensions chamber))
         (dimx (first dimensions))
         (dimy (second dimensions)))
    (loop for x from 0 below dimx
          do (loop for y from 0 below dimy
                   do (format stream "~a" (if (zerop (aref chamber x y))
                                              "."
                                              "#")))
             (format stream "~%"))))

;; To represent the situation, it should be enough to
(defparameter *vertical-chamber*
  (make-instance 'vertical-chamber))

(defparameter *piece-picker*
  (make-instance 'wrapped-sequence :sequence *pieces*))

(defparameter *jet-picker*
  (make-instance 'wrapped-sequence :sequence (read-jet-pattern)))

(defparameter down (complex 0 1))

(defun emptyp (position chamber)
  (zerop (aref chamber (realpart position) (imagpart position))))

(defun within-boudariesp (position chamber)
  (declare (ignore chamber))
  (let ((x (realpart position)))
    (and (> x 0)
         (< x 7))))

(defun can-move (piece delta chamber)
  (loop for element in piece
        always (and (emptyp (+ element delta) chamber)
                    (within-boudariesp (+ element delta) chamber))))

(defun put (piece position)
  (loop for element in piece
        for element-pos = (+ position element)
        do (setf (aref *vertical-chamber* (realpart element-pos) (imagpart element-pos)) 1)))

(defun aoc2022/day17/solution1 ()
  (reset *piece-picker*)
  (loop with ground-level = 7
        repeat 1
        for piece = (next *piece-picker*)
        do (loop repeat 1
                 with position = (complex 2 (- ground-level 3))
                 do (when (can-move piece down *vertical-chamber*)
                      (incf position down))
                    (put piece position))))
