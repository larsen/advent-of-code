(in-package #:advent-of-code)

(defun parse-scratchcard (str)
  (labels ((parse-list-of-integers (str)
             (mapcar #'parse-integer
                     (remove "" (split "\\s+" str)
                             :test 'equalp))))
    (register-groups-bind ((#'parse-integer id)
                           (#'parse-list-of-integers winning-numbers
                                                     my-numbers))
        ("Card\\s+(\\d+): (.*) \\| (.*)" str)
      (list id
            winning-numbers
            my-numbers
            (scratchcard-score winning-numbers my-numbers)
            (matches-count winning-numbers my-numbers)))))

(defun read-scratchcards ()
  (loop for line in (uiop:read-file-lines (asdf:system-relative-pathname
                                           'advent-of-code "inputs/2023/day4"))
        collect (parse-scratchcard line)))

(defun matches-count (winning-numbers my-numbers)
  (loop for n in my-numbers
        count (find n winning-numbers)))

(defun scratchcard-score (winning-numbers my-numbers)
  (if (not (zerop (matches-count winning-numbers my-numbers)))
      (expt 2 (- (matches-count winning-numbers my-numbers) 1))
      0))

(defun aoc2023/day4/solution1 ()
  (loop for (id wn mn score mc) in (read-scratchcards)
        sum score))

(defun aoc2023/day4/solution2 ()
  (let* ((scratchcards (read-scratchcards))
         (card-counters (make-array (length scratchcards)
                                    :initial-element 1)))
    (loop for (id winning-numbers
                  my-numbers
                  score
                  matches-count) in scratchcards
;;          do (format t "~a ~a ~%" id score)
          when (not (zerop matches-count))
            do (loop for delta from 1 to matches-count
                     do (incf (aref card-counters
                                    (+ (- id 1)
                                       delta))
                              (aref card-counters (- id 1))))
          finally (return (reduce #'+ card-counters)))))
