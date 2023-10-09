(in-package #:advent-of-code)

(defstruct fabric-claim
  (id nil :type integer)
  (left nil :type integer)
  (top nil :type integer)
  (width nil :type integer)
  (height nil :type integer))

(defun read-fabric-claims ()
  (loop for l in (uiop:read-file-lines
                  (asdf:system-relative-pathname 'advent-of-code "inputs/2018/day3"))
        collect (register-groups-bind ((#'parse-integer id left top w h))
                   ("#(\\d+) @ (\\d+),(\\d+): (\\d+)x(\\d+)" l)
                 (make-fabric-claim :id id :left left :top top :width w :height h))))

(defun apply-fabric-claim (piece-of-fabric claim)
  (with-slots (left top width height) claim
    (loop for col from left to (+ left (- width 1))
          do (loop for row from top to (+ top (- height 1))
                   do (incf (aref piece-of-fabric row col)) ))))

(defun aoc2018/day3/solution1 ()
  (let ((piece-of-fabric (make-array (list 1000 1000)))
        (counter 0))
    (loop for cl in (read-fabric-claims)
          do (apply-fabric-claim piece-of-fabric cl))
    (loop for col from 0 below 1000
          do (loop for row from 0 below 1000
                   when (>= (aref piece-of-fabric row col) 2)
                     do (incf counter))
          finally (return counter))))

;; ----

(defun apply-fabric-claim-id (piece-of-fabric claim)
  (with-slots (id left top width height) claim
    (loop for col from left to (+ left (- width 1))
          do (loop for row from top to (+ top (- height 1))
                   for (f_id f_writes) = (aref piece-of-fabric row col)
                   when f_id
                     do (setf (aref piece-of-fabric row col)
                              (list id (+ 1 f_writes)))
                   when (not f_id)
                     do (setf (aref piece-of-fabric row col)
                              (list id 1))))))

(defun count-visible-claimed-fabric-area (piece-of-fabric id)
  (let ((counter 0))
    (loop for col from 0 below 1000
          do (loop for row from 0 below 1000
                   when (and (aref piece-of-fabric row col)
                             (= id (car (aref piece-of-fabric row col)))
                             (= 1 (cadr (aref piece-of-fabric row col))))
                     do (incf counter))
          finally (return counter))))

(defun verify-fabric-claim (piece-of-fabric claim)
  (with-slots (id left top width height) claim
    ;; (format t "Verifying ~a~%" id)
    (let ((area (* width height)))
      ;; (format t "Verifying ~a (area: ~a) ~%" id area)
      (= area
         (count-visible-claimed-fabric-area piece-of-fabric id)))))

(defun aoc2018/day3/solution2 ()
  (let ((fabric-claims (read-fabric-claims))
        (piece-of-fabric (make-array (list 1000 1000) :initial-element '())))
    (loop for cl in fabric-claims
          do (apply-fabric-claim-id piece-of-fabric cl))
    (loop for cl in fabric-claims
          when (verify-fabric-claim piece-of-fabric cl)
            return (fabric-claim-id cl))))
