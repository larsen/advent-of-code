(in-package #:advent-of-code)

(defun read-motions ()
  (loop for l in (uiop:read-file-lines
                  (asdf:system-relative-pathname
                   :advent-of-code "inputs/2022/day9"))
        collect (destructuring-bind (dir-raw steps)
                    (split " " l)
                  (cons (intern (string-upcase dir-raw) :advent-of-code)
                        (parse-integer steps)))))

(defstruct vektor
  (x) (y))

(defmethod print-object ((v vektor) stream)
  (format stream "<~d,~d>" (vektor-x v) (vektor-y v)))

(defun dir->vector (dir)
  (cond
    ((eq 'r dir) (make-vektor :x +1 :y 0) )
    ((eq 'l dir) (make-vektor :x -1 :y 0))
    ((eq 'd dir) (make-vektor :x 0 :y -1))
    ((eq 'u dir) (make-vektor :x 0 :y +1))))

(defmethod v+ ((v1 vektor) (v2 vektor))
  (make-vektor :x (+ (vektor-x v1)
                     (vektor-x v2))
               :y (+ (vektor-y v1)
                     (vektor-y v2))))

(defmethod v- ((v1 vektor) (v2 vektor))
  (make-vektor :x (- (vektor-x v1)
                     (vektor-x v2))
               :y (- (vektor-y v1)
                     (vektor-y v2))))

(defmethod delta ((v1 vektor) (v2 vektor))
  (floor (sqrt (+ (expt (- (vektor-x v1)
                           (vektor-x v2)) 2)
                  (expt (- (vektor-y v1)
                           (vektor-y v2)) 2)))))

(defmethod clamped-v- ((v1 vektor) (v2 vektor))
  (make-vektor :x (alexandria:clamp (- (vektor-x v1)
                                       (vektor-x v2))
                                    -1 1)
               :y (alexandria:clamp (- (vektor-y v1)
                                       (vektor-y v2))
                                    -1 1)))

;; .....    .....    .....
;; .....    ..H..    ..H..
;; ..H.. -> ..... -> ..T..
;; .T...    .T...    .....
;; .....    .....    .....
;;
;; .....    .....    .....
;; .....    .....    .....
;; ..H.. -> ...H. -> ..TH.
;; .T...    .T...    .....
;; .....    .....    .....

(defun aoc2022/day9/solution1 ()
  (let* ((motions (read-motions))
         (head (make-vektor :x 0 :y 0))
         (tail (make-vektor :x 0 :y 0))
         (tail-positions (list tail)))
    (loop for (dir . steps) in motions
          do (loop repeat steps
                   do (setf head (v+ head (dir->vector dir)))
                      (if (> (delta head tail) 1)
                          (setf tail (v+ tail (clamped-v- head tail))))
                      (push tail tail-positions)))
    (length (remove-duplicates tail-positions :test #'equalp))))

(defun aoc2022/day9/solution2 ()
  (let* ((motions (read-motions))
         (knots (loop repeat 10
                      collect (make-vektor :x 0 :y 0)))
         (tail-positions (list (car knots))))
    (loop for (dir . steps) in motions
          do (loop repeat steps
                   do (setf (car knots) (v+ (car knots) (dir->vector dir)))
                      (loop for idx from 1 below (length knots)
                            for hd = (nth (- idx 1) knots)
                            for tl = (nth idx knots)
                            when (> (delta hd tl) 1)
                              do (setf (nth idx knots)
                                       (v+ tl (clamped-v- hd tl))))
                      (push (car (last knots)) tail-positions)))
    (length (remove-duplicates tail-positions :test #'equalp))))
