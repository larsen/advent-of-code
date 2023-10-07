(in-package #:advent-of-code)

(defun read-lights-configuration ()
  (let ((file-content (uiop:read-file-lines
                       (asdf:system-relative-pathname 'advent-of-code
                                                      "inputs/2015/day18")))
        (lights-configuration (make-array '(100 100))))
    (loop for l in file-content
          for y from 0
          do (loop for x from 0
                   for c across l
                   when (not (char= c #\Newline))
                     do (setf (aref lights-configuration x y)
                              (if (char= c #\#)
                                  :active
                                  :inactive))))
    lights-configuration))

(defvar +day18-directions+
  '((-1 . -1) (0 . -1) (+1 . -1)
    (-1 .  0)          (+1 .  0)
    (-1 . +1) (0 . +1) (+1 . +1)))

(defun count-neighbours (configuration x y)
  (count :active
         (loop for dir in +day18-directions+
               for x1 = (+ x (car dir))
               for y1 = (+ y (cdr dir))
               collect (handler-case (aref configuration x1 y1)
                         (sb-int:invalid-array-index-error () :error)))))

(defun next-iteration (lights-configuration &key (side 100))
  (let ((new-configuration (make-array (list side side) :initial-element :inactive)))
    (loop for y from 0 below side
          do (loop for x from 0 below side
                   for neighbours = (count-neighbours lights-configuration x y)
                   do (if (eql :active (aref lights-configuration x y))
                          (setf (aref new-configuration x y)
                               (if (or (= 2 neighbours)
                                       (= 3 neighbours))
                                   :active
                                   :inactive))
                          (setf (aref new-configuration x y)
                                (if (= 3 neighbours)
                                   :active
                                   :inactive))))
          finally (return new-configuration))))

(defun aoc2015/day18/count-lights-on (lights-configuration)
  (loop with counter = 0
        for y from 0 below 100
        do (loop for x from 0 below 100
                 when (eql :active (aref lights-configuration x y))
                   do (incf counter))
        finally (return counter)))

(defun aoc2015/day18/solution1 ()
  (loop with lights-configuration = (read-lights-configuration)
        repeat 100
        do (setf lights-configuration (next-iteration lights-configuration))
        finally (return (aoc2015/day18/count-lights-on lights-configuration))))

(defun next-iteration-stuck-corner (lights-configuration &key (side 100))
  (let ((new-configuration (make-array (list side side) :initial-element :inactive)))
    (loop for y from 0 below side
          do (loop for x from 0 below side
                   for neighbours = (count-neighbours lights-configuration x y)
                   do (if (or (and (= x 0) (= y 0))
                              (and (= x 99) (= y 0))
                              (and (= x 0) (= y 99))
                              (and (= x 99) (= y 99)))
                          (setf (aref new-configuration x y) :active)
                          (if (eql :active (aref lights-configuration x y))
                              (setf (aref new-configuration x y)
                                    (if (or (= 2 neighbours)
                                            (= 3 neighbours))
                                        :active
                                        :inactive))
                              (setf (aref new-configuration x y)
                                    (if (= 3 neighbours)
                                        :active
                                        :inactive)))))
          finally (return new-configuration))))

(defun aoc2015/day18/solution2 ()
    (loop with lights-configuration = (read-lights-configuration)
        repeat 100
        do (setf lights-configuration (next-iteration-stuck-corner lights-configuration))
        finally (return (aoc2015/day18/count-lights-on lights-configuration))))
