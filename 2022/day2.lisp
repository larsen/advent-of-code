(in-package #:advent-of-code)

(defun read-strategy-book ()
  (mapcar (lambda (l)
            (mapcar (lambda (str)
                      (intern (string-upcase str) :advent-of-code))
                    (split-sequence #\Space l)))
          (uiop:read-file-lines (asdf:system-relative-pathname
                                 :advent-of-code "inputs/2022/day2"))))

(defgeneric game-score (player1-shape player2-shape))
(defgeneric shape-for-outcome (player1-shape outcome))

;; A for Rock, B for Paper, and C for Scissors.
;; X for Rock, Y for Paper, and Z for Scissors

;; (1 for Rock, 2 for Paper, and 3 for Scissors)
;; 0 if you lost, 3 if the round was a draw, and 6 if you won

;; X means you need to lose, Y means you need to end the round in a draw

(defmacro define-games-scores (scores)
  `(progn
     ,@(loop for (player1-shape player2-shape/outcome score shape-to-play) in scores
             collect `(defmethod game-score ((p1s (eql ',player1-shape))
                                             (p2s (eql ',player2-shape/outcome))) ,score)
             collect `(defmethod shape-for-outcome ((p1s (eql ',player1-shape))
                                                    (oc (eql ',player2-shape/outcome))) ',shape-to-play))))

(define-games-scores
    ((A X 4 Z)
     (A Y 8 X)
     (A Z 3 Y)
     (B X 1 X)
     (B Y 5 Y)
     (B Z 9 Z)
     (C X 7 Y)
     (C Y 2 Z)
     (C Z 6 X)))

(defun aoc2022/day2/solution1 ()
  (let ((strategy (read-strategy-book)))
    (loop for (player1-shape player2-shape) in strategy
          sum (game-score player1-shape player2-shape))))

(defun aoc2022/day2/solution2 ()
  (let ((strategy (read-strategy-book)))
    (loop for (player1-shape outcome) in strategy
          sum (game-score player1-shape
                          (shape-for-outcome player1-shape outcome)))))
