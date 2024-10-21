(in-package #:advent-of-code)

(defstruct unit
  (kind)
  (attack-power 3)
  (hit-points 200)
  (row)
  (col))

(defmethod unit-pos ((u unit))
  (cons (unit-row u)
        (unit-col u)))

(defgeneric in-reading-order (a b))
(defmethod in-reading-order ((a unit) (b unit))
  (if (= (unit-row a) (unit-row b))
      (< (unit-col a) (unit-col b))
      (< (unit-row a) (unit-row b))))

(defmethod in-reading-order (ca cb)
  (if (= (car ca) (car cb))
      (< (cdr ca) (cdr cb))
      (< (car ca) (car cb))))

(defstruct beverage-bandits
  (cavern)
  (units))

(defmethod print-object ((bb beverage-bandits) stream)
  (format stream "<Cavern: ~a Units: ~a>"
          (array-dimensions (beverage-bandits-cavern bb))
          (length (beverage-bandits-units bb))))

(defmethod print-object ((u unit) stream)
  (format stream "<~a: ~a,~a>"
          (string (unit-kind u))
          (unit-row u)
          (unit-col u)))

(defun read-beverage-bandits ()
  (let* ((raw-input (read-input-file-as-lines "inputs/2018/day15"))
         (cavern-array (make-array (list (length raw-input)
                                         (length (first raw-input)))
                                   :initial-contents raw-input))
         (cavern-dims (array-dimensions cavern-array))
         (units (loop for row from 0 below (first cavern-dims)
                      append (loop for col from 0 below (second cavern-dims)
                                   when (char= (aref cavern-array row col) #\G)
                                     collect (make-unit :kind :goblin :row row :col col)
                                   when (char= (aref cavern-array row col) #\E)
                                     collect (make-unit :kind :elf :row row :col col)))))
    (make-beverage-bandits
     :cavern cavern-array
     :units units)))

(defmethod identify-targets ((u unit) (bb beverage-bandits))
  (loop for e in (beverage-bandits-units bb)
        when (not (eq (unit-kind e) (unit-kind u)))
          collect e))

(defmethod adjacent-squares ((u unit) (bb beverage-bandits))
  (let ((directions '((0 . -1) (-1 . 0) (1 . 0) (0 . 1))))
    (loop for d in directions
          for (row col) = (list (+ (car d) (unit-row u))
                                (+ (cdr d) (unit-col u)))
          when (array-in-bounds-p (beverage-bandits-cavern bb) row col)
            collect (cons row col))))

(defmethod adjacent-open-squares ((u unit) (bb beverage-bandits))
  (loop for (row . col) in (adjacent-squares u bb)
        when (char= #\. (aref (beverage-bandits-cavern bb) row col))
          collect (cons row col)))

(defmethod adjacent-enemy-squares ((u unit) (bb beverage-bandits))
  (loop for (row . col) in (adjacent-squares u bb)
        when (cond
               ((eq (unit-kind u) :goblin)
                (char= #\E (aref (beverage-bandits-cavern bb) row col)))
               ((eq (unit-kind u) :elf)
                (char= #\G (aref (beverage-bandits-cavern bb) row col))))
          collect (cons row col)))

(defgeneric in-range-of-targets (unit beverage-bandits))
(defmethod in-range-of-targets ((u unit) (bb beverage-bandits))
  (let ((targets (identify-targets u bb)))
    (when targets
      (first (sort
              (remove-if-not (lambda (target)
                               (some (lambda (pos)
                                       (and (= (car pos) (unit-row u))
                                            (= (cdr pos) (unit-col u))))
                                     (adjacent-open-squares target bb)))
                             targets)
              #'in-reading-order)))))

(defmethod closest-target-range ((u unit) (bb beverage-bandits))
  (let ((cavern (beverage-bandits-cavern bb)))
    (multiple-value-bind (l distances)
        (general-shortest-path cavern
                               :start (unit-pos u)
                               :goal (cons 0 0)
                               :valid-p (lambda (dest src map)
                                          (declare (ignore src))
                                          (char= (aref map (car dest) (cdr dest))
                                                 #\.)))
      (declare (ignore l))
      (let* ((targets (identify-targets u bb))
             (in-range-of-targets (loop for target in targets
                                        append (adjacent-open-squares target bb)))
             (reachable (remove-if-not (lambda (pos)
                                         (< (aref distances (car pos) (cdr pos))
                                            infinity))
                                       in-range-of-targets))
             (nearest (sort reachable #'<
                            :key (lambda (pos)
                                   (aref distances (car pos) (cdr pos)))))
             (grouped-by-distance (let ((h (make-hash-table)))
                                    (loop for n in nearest
                                          for d = (aref distances (car n) (cdr n))
                                          do (push n (gethash d h '()))
                                          finally (return h))))
             (min-distance (loop for d being the hash-keys of grouped-by-distance
                                 minimizing d)))
        (first (sort (gethash min-distance grouped-by-distance)
                     #'in-reading-order))))))

(defmethod compute-unit-move ((u unit) (bb beverage-bandits))
  ;; Try every possible move (there are at most 4)
  ;; See if it brings the unit closer to the target square.
  ;; If there's a tie, choose the first one in reading order
  (let ((target-square (closest-target-range u bb)))
    (multiple-value-bind (_ distance-matrix)
        (general-shortest-path
           (beverage-bandits-cavern bb)
           :start target-square
           :goal (unit-pos u)
           :valid-p (lambda (dest src cavern)
                      (declare (ignore src))
                      (char= (aref cavern (car dest) (cdr dest))
                             #\.)))
      (declare (ignore _))
      (loop with best-candidate
            with best-candidate-distance = infinity
            for candidate in (adjacent-open-squares u bb)
            for candidate-distance = (aref distance-matrix
                                           (car candidate)
                                           (cdr candidate))
            when (< candidate-distance best-candidate-distance)
              do (setf best-candidate candidate
                       best-candidate-distance candidate-distance)
            finally (return best-candidate)))))

(defun sketch-of-cavern (cavern units cell-size)
  (let ((cavern-dims (array-dimensions cavern)))
    (loop for row from 0 below (first cavern-dims)
          do (loop for col from 0 below (second cavern-dims)
                   for color = (case (aref cavern row col)
                                 (#\# (rgb .25 .15 .05))
                                 (#\. +gray+)
                                 ;; These are cells occupied by a unit in the
                                 ;; initial schema. We'll take care of drawing units
                                 ;; in the next LOOP form.
                                 (otherwise +gray+))
                   do (with-pen (make-pen :fill color :stroke +white+)
                        (rect (* col cell-size)
                              (* row cell-size)
                              (- cell-size 1)
                              (- cell-size 1)))))
    (loop for u in units
          for color = (case (unit-kind u)
                        (:goblin +magenta+)
                        (:elf (rgb 0 .7 0)))
          do (with-pen (make-pen :fill color :stroke +white+)
               (rect (* (unit-col u) cell-size)
                     (* (unit-row u) cell-size)
                     (- cell-size 1)
                     (- cell-size 1)))
             (with-font (make-font :size (* cell-size .65) :color +white+)
               (text (format nil "~a" (unit-hit-points u))
                     (* (unit-col u) cell-size)
                     (* (unit-row u) cell-size)
                     )))))

(defun sketch-of-targets (selected-unit bb cell-size)
  (with-pen (make-pen :fill +red+ :stroke +black+)
    (loop for target in (identify-targets selected-unit bb)
          do (rect (+ (* (unit-col target) cell-size))
                   (+ (* (unit-row target) cell-size))
                   cell-size
                   cell-size)
             (with-pen (make-pen :fill +yellow+ :stroke +white+)
               (loop for s in (adjacent-open-squares target bb)
                     do (circle (+ (* cell-size (cdr s))
                                   (/ cell-size 2))
                                (+ (* cell-size (car s))
                                   (/ cell-size 2))
                                (/ cell-size 4)))))))

(defun sketch-of-distances-to-empty-cells (selected-unit cavern cell-size)
  (let ((cavern-dims (array-dimensions cavern)))
    (multiple-value-bind (l distances)
        (general-shortest-path cavern
                               :start (unit-pos selected-unit)
                               :goal (cons 0 0)
                               :valid-p (lambda (dest src map)
                                          (declare (ignore src))
                                          (char= (aref map
                                                       (car dest)
                                                       (cdr dest))
                                                 #\.)))
      (declare (ignore l))
      (loop for row from 0 below (first cavern-dims)
            do (loop for col from 0 below (second cavern-dims)
                     for d = (aref distances row col)
                     do (when (< d infinity)
                          (with-font (make-font :size (* cell-size .6)
                                                :color +white+
                                                :align :center)
                            (text (format nil "~a" d)
                                  (+ (* col cell-size)
                                     (/ cell-size 2))
                                  (+ (* row cell-size)))))
                        (with-pen (make-pen :fill (rgb 1 1 0
                                                       (/ 1 (+ d .001))))
                          (rect (+ (* col cell-size))
                                (+ (* row cell-size))
                                cell-size
                                cell-size)))))))

(defun sketch-of-closest-in-range-cell (closest-in-range-target-cell cell-size)
  (when closest-in-range-target-cell
    (with-pen (make-pen :fill +green+)
      (star 5 (+ (* (cdr closest-in-range-target-cell) cell-size)
                 (/ cell-size 2))
              (+ (* (car closest-in-range-target-cell) cell-size)
                 (/ cell-size 2))
              (/ cell-size 3)
              (/ cell-size 3)))))

(defsketch beverage-bandits-sketch ((bb nil)
                                    (turn 0)
                                    (selected-unit nil)
                                    (should-save nil)
                                    (width 750)
                                    (height 750)
                                    (margin 50))
  (background +white+)
  (let* ((cavern (beverage-bandits-cavern bb))
         (units (beverage-bandits-units bb))
         (cavern-dims (array-dimensions cavern))
         (cell-size (floor (/ (- width margin) (first cavern-dims))))
         (padding-x (/ (- width (* cell-size (second cavern-dims))) 2))
         (padding-y (/ (- width (* cell-size (first cavern-dims))) 2)))

    (with-translate (padding-x padding-y)
      (sketch-of-cavern cavern units cell-size)

      (when selected-unit
        ;; Display targets and their adjacent squares
        (sketch-of-targets selected-unit bb cell-size)

        ;; Display adjacent open squares
        (with-pen (make-pen :fill +white+ :stroke +white+)
          (loop for s in (adjacent-open-squares selected-unit bb)
                do (circle (+ (* cell-size (cdr s))
                              (/ cell-size 2))
                           (+ (* cell-size (car s))
                              (/ cell-size 2))
                           (/ cell-size 4))))

        ;; Display distances to all empty cells
        (sketch-of-distances-to-empty-cells selected-unit cavern cell-size)

        ;; Display the closest in-range cell
        (let ((closest-in-range-target-cell (closest-target-range
                                             selected-unit bb)))
          (sketch-of-closest-in-range-cell
           closest-in-range-target-cell cell-size)
          (with-font (make-font :size 15)
            (text (format nil "Unit ~a, enemy closest range cell: <row ~a, col ~a>"
                          selected-unit
                          (car closest-in-range-target-cell)
                          (cdr closest-in-range-target-cell))
                  ;; FIXME I don't like these negative coordinates,
                  ;; due to the fact we're in a with-translate context
                  -20 -20)))

        ;; Display the move
        (let ((move (compute-unit-move selected-unit bb)))
          (with-pen (make-pen :stroke +red+ :fill +red+)
            (rect (* cell-size (cdr move))
                  (* cell-size (car move))
                  10 10)))

        ))

    (with-font (make-font :size 15)
      (text (format nil "Turn: ~a  Score: ~a --" turn 0) 20 0))
    (when should-save
      (setf should-save nil)
      (save-png "/tmp/vis.png"))))


(defmethod unit-attack ((u unit) (bb beverage-bandits))
  ;; To attack, the unit first determines all of the targets that are
  ;; in range of it by being immediately adjacent to it.
  ;; If there are
  ;; no such targets, the unit ends its turn.
  ;; Otherwise, the adjacent
  ;; target with the fewest hit points is selected; in a tie, the
  ;; adjacent target with the fewest hit points which is first in
  ;; reading order is selected.
  (let* ((enemy-squares (adjacent-enemy-squares u bb))
         (enemies (loop for s in enemy-squares
                        collect (find-if (lambda (e)
                                           (and (= (unit-row e) (car s))
                                                (= (unit-col e) (cdr s))))
                                         (beverage-bandits-units bb)))))
    enemies))

(defun beverage-bandits-step (bb)
  (setf (beverage-bandits-units bb)
        (sort (beverage-bandits-units bb) #'in-reading-order))
  (beverage-bandits-units bb)
  (loop for u in (beverage-bandits-units bb)
        for targets = (identify-targets u bb)
        for in-range-squares = (in-range-of-targets u bb)
        do (if (not in-range-squares)
               (let ((move (compute-unit-move u bb)))
                 (setf (unit-row u) (car move)
                       (unit-col u) (cdr move))))
           (unit-attack u bb)
        until (emptyp targets)
        finally (return bb)))

(defmethod on-text ((window beverage-bandits-sketch) text)
  (when (string= text "s")
    (with-slots (should-save) window
      (setf should-save T)))
  (when (string= text " ")
    (with-slots (bb turn selected-unit) window
      (incf turn)
      (setf bb (beverage-bandits-step bb)))))

(defmethod on-click ((window beverage-bandits-sketch) x y)
  (with-slots (width margin bb selected-unit) window
    (let* ((cavern (beverage-bandits-cavern bb))
           (units (beverage-bandits-units bb))
           (cavern-dims (array-dimensions cavern))
           (cell-size (floor (/ (- width margin) (first cavern-dims))))
           (padding-x (/ (- width (* cell-size (second cavern-dims))) 2))
           (padding-y (/ (- width (* cell-size (first cavern-dims))) 2))
           (ax (floor (/ (- x padding-x) cell-size)))
           (ay (floor (/ (- y padding-y) cell-size)))
           (matching-units (remove-if-not (lambda (u)
                                            (and (= ax (unit-col u))
                                                 (= ay (unit-row u)))) units)))
      (setf selected-unit (first matching-units)))))

(defun aoc2018/day15/solution1 ()
  )

(defun aoc2018/day15/solution2 ()
  )
