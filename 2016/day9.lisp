(in-package #:advent-of-code)

(defun read-compressed-file ()
  (read-input-file "inputs/2016/day9"))

(defun decompress (string)
  (apply #'concatenate 'string
         (let ((pos 0))
           (loop for next-elem = (subseq string pos (+ pos 1))
                 when (not (string= next-elem "("))
                   do (incf pos) and collect next-elem
                 when (string= next-elem "(")
                   append (destructuring-bind (match regs)
                              (multiple-value-list
                               (scan-to-strings "\\((\\d+)x(\\d+)\\)" (subseq string pos)))
                            (if match
                                (progn
                                  (incf pos (length match))
                                  (let* ((len (parse-integer (aref regs 0)))
                                         (repetitions (parse-integer (aref regs 1)))
                                         (result
                                           (loop repeat repetitions
                                                 collect (subseq string pos (+ pos len)))))
                                    (incf pos len)
                                    result))
                                (progn
                                  (incf pos)
                                  next-elem)))
                 until (= pos (length string))))))

(defun aoc2016/day9/solution1 ()
  (length
   (string-trim '(#\Newline) (decompress (read-compressed-file)))))

;; (27x12)(20x12)(13x14)(7x10)(1x12)A
;; (* 12 10 14 12 12 1)

;; (25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN
;; (25x3) -->
;;   (3x3)ABC -> 9
;;   (2x3)XY -> 6
;;   (5x2)PQRSTX -> 10
;; (18x9) -->
;;   (3x2)TWO -> 6
;;   (5x7)SEVEN -> 35
;;
;; (+
;;  (* 3 (+ (* 3 3) (* 2 3) (* 5 2)))
;;  1
;;  (* 9 (+ (* 5 7)
;;          (* 3 2))))

(defun multi-decompress (string)
  (declare (ignore string)))

(defun aoc2016/day9/solution2 ()
  )
