(in-package #:advent-of-code)

(defstruct guard
  (x)
  (y)
  (heading :north)
  (has-left nil))

(defun read-gallivant-map ()
  (let* ((raw (read-input-file-as-lines "inputs/2024/day6"))
         (height (length raw))
         (width (length (first raw)))
         (gallivant-map (make-array (list height width) :initial-contents raw)))
    gallivant-map))

(defun ensure-guard-in-map (gallivant-map)
  (let* ((dims (array-dimensions gallivant-map))
         (height (first dims))
         (width (second dims))
         (guard))
    (loop for row from 0 below height
          do (loop for col from 0 below width
                   when (char= #\^ (aref gallivant-map row col))
                     do (setf guard (make-guard :x col :y row)))
          finally (return guard))))

(defsketch gallivant-map ((lab-map (read-gallivant-map))
                          (guard (ensure-guard-in-map lab-map))
                          (grid-size 3)
                          (obstacle-pen (make-pen :stroke (rgb 0 0 0)
                                                  :fill (rgb 0 0 0)))
                          (guard-pen (make-pen :stroke (rgb 1 0 0)
                                               :fill (rgb 1 0 0))))
  (background (rgb 1 1 1))
  ;; Draw obstacles
  (let* ((dims (array-dimensions lab-map))
         (height (first dims))
         (width (second dims)))
    (with-pen obstacle-pen
      (loop for row from 0 below height
            do (loop for col from 0 below width
                     when (char= #\# (aref lab-map row col))
                       do (rect (* grid-size col) (* grid-size row)
                                grid-size grid-size)))))
  (with-pen guard-pen
    (rect (* grid-size (guard-x guard))
          (* grid-size (guard-y guard))
          grid-size grid-size)))

(defmethod in-lab-map-bounds ((guard guard) lab-map)
  (array-in-bounds-p lab-map (guard-y guard) (guard-x guard)))

(defmethod get-new-walking-position ((guard guard))
  (with-slots (x y heading) guard
    (case heading
      (:north (cons x (- y 1)))
      (:south (cons x (+ y 1)))
      (:west  (cons (- x 1) y))
      (:east  (cons (+ x 1) y)))))

(defmethod turn-right ((guard guard))
  (setf (guard-heading guard)
        (case (guard-heading guard)
          (:north :east)
          (:south :west)
          (:west  :north)
          (:east  :south))))

(defmethod walk ((guard guard) lab-map)
  (let ((new-pos (get-new-walking-position guard)))
    (cond
      ((not (array-in-bounds-p lab-map (cdr new-pos) (car new-pos)))
       (setf (guard-has-left guard) T))
      ((and (array-in-bounds-p lab-map (cdr new-pos) (car new-pos))
            (not (char= #\# (aref lab-map (cdr new-pos) (car new-pos)))))
       (progn (setf (guard-x guard) (car new-pos))
              (setf (guard-y guard) (cdr new-pos))))
      (T (turn-right guard) ))))

(defun aoc2024/day6/solution1 ()
  (let* ((lab-map (read-gallivant-map))
         (guard (ensure-guard-in-map lab-map))
         (positions (list (cons (guard-x guard)
                                (guard-y guard)))))
    (loop while (not (guard-has-left guard))
          do (walk guard lab-map)
             (signal 'guard-moved :x (guard-x guard) :y (guard-y guard))
             (push (cons (guard-x guard) (guard-y guard))
                   positions)
          finally (return (length (unique positions :test 'equal))))))

(define-condition guard-moved ()
  ((x :initarg :x :accessor guard-moved-x)
   (y :initarg :y :accessor guard-moved-y)))

(defun gallivant-simulation ()
  (let ((gm (make-instance 'gallivant-map)))
    (handler-bind ((guard-moved
                     (lambda (g)
                       (setf (guard-x (gallivant-map-guard gm))
                             (guard-moved-x g))
                       (setf (guard-y (gallivant-map-guard gm))
                             (guard-moved-y g))
                       (sleep 0.05))))
      (aoc2024/day6/solution1))))

(defun aoc2024/day6/solution2 ()
  )
