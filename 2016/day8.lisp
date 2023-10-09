(in-package #:advent-of-code)

(defun read-screen-instructions ()
  (read-input-file-as-lines "inputs/2016/day8"))

(defparameter *screen* nil)

(defun reset-screen ()
  (setf *screen* (make-array '(50 6) :element-type 'boolean
                                     :initial-element nil)))
(defun print-screen (screen)
  (loop for y from 0 to 5
        do (loop for x from 0 to 49
                 do (format t "~:[.~;#~]" (aref screen x y)))
           (format t "~%")))

(defun count-pixel (screen)
  (loop with count = 0
        for y from 0 to 5
        do (loop for x from 0 to 49
                 when (aref screen x y)
                   do (incf count))
        finally (return count)))

(defun rect (w h)
  (loop for y from 0 to (- h 1)
        do (loop for x from 0 to (- w 1)
                 do (setf (aref *screen* x y) t))))

(defun rotate-row-1 (row)
  (let ((new-row (make-array 50 :element-type 'boolean
                                :initial-element nil)))
    (loop for idx from 0 below 50
          do (let ((new-idx (if (< idx 49)
                                (+ idx 1)
                                0)))
               (setf (aref new-row new-idx)
                     (aref *screen* idx row))))
    (loop for idx from 0 below 50
          do (setf (aref *screen* idx row)
                   (aref new-row idx)))))

(defun rotate-row (row n)
  (loop repeat n
        do (rotate-row-1 row)))

(defun rotate-col-1 (col)
  (let ((new-col (make-array 6 :element-type 'boolean
                               :initial-element nil)))
    (loop for idx from 0 below 6
          do (let ((new-idx (if (< idx 5)
                                (+ idx 1)
                                0)))
               (setf (aref new-col new-idx)
                     (aref *screen* col idx))))
    (loop for idx from 0 below 6
          do (setf (aref *screen* col idx)
                   (aref new-col idx)))))

(defun rotate-col (col n)
    (loop repeat n
        do (rotate-col-1 col)))

;; "rect (\d+)x(\d+)"
;; "rotate column x=(\d+) by (\d+)"
;; "rotate row y=(\d+) by (\d+)"
(defun aoc2016/day8/solution1 ()
  (reset-screen)
  (loop for instr in (read-screen-instructions)
        do (register-groups-bind ((#'parse-integer w)
                                  (#'parse-integer h))
               ("rect (\\d+)x(\\d+)" instr)
             (rect w h))
           (register-groups-bind ((#'parse-integer col)
                                  (#'parse-integer n))
               ("rotate column x=(\\d+) by (\\d+)" instr)
             (rotate-col col n))
           (register-groups-bind ((#'parse-integer row)
                                  (#'parse-integer n))
               ("rotate row y=(\\d+) by (\\d+)" instr)
             (rotate-row row n))
        finally
           (return (count-pixel *screen*))))

(defun aoc2016/day8/solution2 ()
  ; Should I "ocr" the grid?
  "ZFHFSFOGPO")
