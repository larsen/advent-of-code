(in-package #:advent-of-code)

(defun read-engine-schematic ()
  (let ((lines (uiop:read-file-lines (asdf:system-relative-pathname
                                      'advent-of-code "inputs/2023/day3"))))
    (make-array (list (length lines)
                      (length (first lines)))
                :initial-contents lines)))

(defun is-adjacent-to-symbol-p (row col schematic)
  (loop for (drow dcol) in '((-1 -1)
                             (-1 0)
                             (-1 1)
                             (0 -1)
                             (0 +1)
                             (+1 -1)
                             (+1 0)
                             (+1 +1))
        for adj-row = (+ row drow)
        for adj-col = (+ col dcol)
        when (and (array-in-bounds-p schematic adj-row adj-col)
                  (not (digit-char-p (aref schematic adj-row adj-col)))
                  (not (char= #\. (aref schematic adj-row adj-col))))
          collect (make-engine-part :symbol (aref schematic adj-row adj-col)
                                    :row adj-row
                                    :col adj-col)))

(defstruct engine-part
  (symbol)
  (row)
  (col))

(defun part-numbers (schematic)
  (loop with (rows cols) = (array-dimensions schematic)
        for row from 0 below rows
        append (loop with n = 0
                     with in-number = nil
                     with adjacent-to-symbol = nil
                     with adjacent-engine-parts = '()
                     for col from 0 below cols
                     for c = (aref schematic row col)
                     when (digit-char-p c)
                       do (setf in-number T)
                          (setf n (+ (* n 10)
                                     (digit-char-p c)))
                          (setf adjacent-to-symbol
                                (or adjacent-to-symbol
                                    (not (null
                                          (is-adjacent-to-symbol-p row col schematic)))))
                          (setf adjacent-engine-parts
                                (append adjacent-engine-parts
                                        (is-adjacent-to-symbol-p row col schematic)))
                     when (and (or (not (digit-char-p c))
                                   (= col (- cols 1)))
                               in-number)
                       collect (list n adjacent-to-symbol
                                     (unique adjacent-engine-parts
                                             :test 'equalp))
                       and do (setf n 0)
                              (setf in-number nil)
                              (setf adjacent-to-symbol nil)
                              (setf adjacent-engine-parts '()))))

(defun aoc2023/day3/solution1 ()
  (sum (mapcar #'first (remove-if-not #'second (part-numbers (read-engine-schematic))))))

(defun aoc2023/day3/solution2 ()
  (let ((engine-parts (make-hash-table :test 'equalp)))
    (loop for (n has-adj-symbol adj-symbols) in (part-numbers (read-engine-schematic))
          do (loop for s in adj-symbols
                   do (push n (gethash s engine-parts))))
    (loop for s being the hash-keys of engine-parts
          for part-numbers being the hash-values of engine-parts
          when (and (char= #\* (engine-part-symbol s))
                    (= 2 (length part-numbers)))
            sum (apply #'* part-numbers))))
