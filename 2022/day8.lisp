(in-package #:advent-of-code)

;;        N
;;        ^
;;      30373
;;      25512
;;  E < 65332 > W
;;      33549
;;      35390
;;        V
;;        S


(defun read-forest ()
  (let ((forest-raw (loop for l in (uiop:read-file-lines
                                    (asdf:system-relative-pathname
                                     :advent-of-code "inputs/2022/day8"))
                          collect (mapcar #'parse-integer (split "" l)))))
    (make-array (list (length forest-raw)
                      (length (car forest-raw)))
                :initial-contents forest-raw)))

(defun trees-heights-from-point (row col forest dir)
  (let* ((shape (array-dimensions forest))
         (max-row (first shape))
         (max-col (second shape)))
    (case dir
      (:north (loop for r from row downto 0 collect (aref forest r col)))
      (:east (loop for c from col downto 0 collect (aref forest row c)))
      (:south (loop for r from row below max-row collect (aref forest r col)))
      (:west (loop for c from col below max-col collect (aref forest row c))  ))))

(defun visible-from-dir-p (row col forest dir)
  (loop for tree-height in (cdr (trees-heights-from-point row col forest dir))
        always (< tree-height (aref forest row col))))

(defun visible-p (row col forest)
  (loop for dir in '(:north :east :south :west)
          thereis (visible-from-dir-p row col forest dir)))

(defun aoc2022/day8/solution1 ()
  (let* ((forest (read-forest))
         (shape (array-dimensions forest))
         (max-row (first shape))
         (max-col (second shape)))
    (loop with counter = 0
          for row from 0 below max-row
          do (loop for col from 0 below max-col
                   when (visible-p row col forest)
                     do (incf counter))
          finally (return counter))))

(defun scenic-distance-dir (row col forest dir)
  (let ((counter 0))
    (loop for tree-height in (cdr (trees-heights-from-point row col forest dir))
          do (incf counter)
          until (>= tree-height (aref forest row col))
          finally (return counter))))

(defun scenic-distance (row col forest)
  (reduce #'* (loop for dir in '(:north :east :south :west)
                    collect (scenic-distance-dir row col forest dir))))

(defun aoc2022/day8/solution2 ()
  (let* ((forest (read-forest))
         (shape (array-dimensions forest))
         (max-row (first shape))
         (max-col (second shape))
         (max-scenic-distance 0))
    (loop for row from 0 below max-row
          do (loop for col from 0 below max-col
                   do (setf max-scenic-distance
                            (max max-scenic-distance
                                 (scenic-distance row col forest))))
          finally (return max-scenic-distance))))
