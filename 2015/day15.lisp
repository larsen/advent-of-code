(in-package #:advent-of-code)

(defun read-ingredients ()
  (let ((ingredients '())
        (re "(\\w+): capacity (-?\\d+), durability (-?\\d+), flavor (-?\\d+), texture (-?\\d+), calories (-?\\d+)"))
    (loop for idx from 0
          for line in (uiop:read-file-lines
                       (asdf:system-relative-pathname 'advent-of-code
                                                      "inputs/2015/day15"))
          do (register-groups-bind (name
                                    (#'parse-integer capacity)
                                    (#'parse-integer durability)
                                    (#'parse-integer flavor)
                                    (#'parse-integer texture)
                                    (#'parse-integer calories))
                 (re line)
               (push `((:name . ,name)
                       (:index . ,idx)
                       (:capacity . ,capacity)
                       (:durability . ,durability)
                       (:flavor . ,flavor)
                       (:texture . ,texture)
                       (:calories . ,calories))
                     ingredients))
          finally (return ingredients))))

(defun ingredient-matrix (ingredients)
  (magicl:from-list
   (loop for property in '(:capacity :durability :flavor :texture)
         append (loop for ing in ingredients
                      collect (coerce (cdr (assoc property ing)) 'double-float)))
   (list 4 (length ingredients))))

(defun cookie-score (cookie)
  ;; Cookie is represented as a 4x1 matrix of floats
  (let ((array (magicl:lisp-array cookie)))
    (reduce #'* (loop for i from 0 below 4
                      for property = (aref array i 0)
                      collect (if (< property 0)
                                  0
                                  property)))))

(defun aoc2015/day15/solution1-test ()
  (let ((ingredient-matrix (ingredient-matrix (read-ingredients))))
    (loop with max-score = 0
          for i from 1d0 below 100d0
          do (loop for j from 1d0 below 100d0
                   when (= (+ i j) 100)
                     do (let ((score (cookie-score (magicl:@ ingredient-matrix
                                                       (magicl:from-list (list i j)
                                                                         '(2 1))))))
                          (if (> score max-score)
                              (setf max-score score))))
          finally (return max-score))))

(defun aoc2015/day15/solution1 ()
  (let ((ingredient-matrix (ingredient-matrix (read-ingredients))))
    (loop with max-score = 0
          for i from 1d0 below 100d0
          do (loop for j from 1d0 below 100d0
                   do (loop for k from 1d0 below 100d0
                            do (loop for l from 1d0 below 100d0
                                     when (= (+ i j k l) 100)
                                       do (let ((score (cookie-score (magicl:@ ingredient-matrix
                                                                         (magicl:from-list (list i j k l)
                                                                                           '(4 1))))))
                                            (if (> score max-score)
                                                (setf max-score score))))))
          finally (return max-score))))

(defun ingredient-calories (ingredients)
  (loop for ing in ingredients
        collect (cdr (assoc :calories ing))))

(defun cookie-calories (ingredients calories)
  (reduce #'+ (loop for i in ingredients
                    for c in calories
                    collect (* i c))))

(defun aoc2015/day15/solution2 ()
  (let* ((ingredients (read-ingredients))
         (ingredient-calories (ingredient-calories ingredients))
         (ingredient-matrix (ingredient-matrix ingredients)))
    (loop with max-score = 0
          for i from 1d0 below 100d0
          do (loop for j from 1d0 below 100d0
                   do (loop for k from 1d0 below 100d0
                            do (loop for l from 1d0 below 100d0
                                     when (and (= (+ i j k l) 100)
                                               (= 500 (cookie-calories (list i j k l)
                                                                       ingredient-calories)))
                                       do (let ((score (cookie-score (magicl:@ ingredient-matrix
                                                                         (magicl:from-list (list i j k l)
                                                                                           '(4 1))))))
                                            (if (> score max-score)
                                                (setf max-score score))))))
          finally (return max-score))))
