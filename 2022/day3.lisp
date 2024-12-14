(in-package #:advent-of-code)

(defun read-rucksacks ()
  (uiop:read-file-lines (asdf:system-relative-pathname
                         :advent-of-code "inputs/2022/day3")))

(defun compartments (rucksack)
  (let ((mid (/ (length rucksack) 2)))
    (list (subseq rucksack 0 mid)
          (subseq rucksack mid))))

(defun priority (item)
  ;; Lowercase item types a through z have priorities 1 through 26.
  ;; Uppercase item types A through Z have priorities 27 through 52.
  (cond ((char= item (char-downcase item)) (- (char-code item) 96) )
        (t (- (char-code item) 38) )))

(defun aoc2022/day3/solution1 ()
  (loop for r in (read-rucksacks)
        for (c1 c2) = (compartments r)
        sum (priority (car (intersection (coerce c1 'list)
                                         (coerce c2 'list))))))

(defun chunks (lst n)
  (labels ((%chunks (lst n current-chunk chunks)
             (cond ((null lst) (reverse (cons (reverse current-chunk) chunks)))
                   ((= (length current-chunk) n) (%chunks lst n '() (cons (reverse current-chunk) chunks)))
                   (t (%chunks (cdr lst) n (cons (car lst) current-chunk) chunks)))
             ))
    (%chunks lst n '() '())))

(defun aoc2022/day3/solution2 ()
  (loop with rucksacks = (read-rucksacks)
        ;; Alternatively, we could use BATCHES from Serapeum
        for group in (chunks rucksacks 3)
        sum (priority (car (intersection (coerce (first group) 'list)
                                         (intersection (coerce (second group) 'list)
                                                       (coerce (third group) 'list)))))))
