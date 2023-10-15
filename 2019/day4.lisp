(in-package #:advent-of-code)

(defvar +aoc2019/day4/input+ "246540-787419")

(defun compute-range-limits (note)
  (register-groups-bind ((#'parse-integer min max))
      ("\(\\d+\)-\(\\d+\)" note)
    (list min max)))

(defun two-adj-digits-are-the-same (n)
  (loop for (a b) on (coerce (format nil "~a" n) 'list)
        while b thereis (char= a b)))

(defun digits-never-decrease (n)
  (loop for (a b) on (coerce (format nil "~a" n) 'list)
        while b always (char-not-greaterp a b)))

(defun aoc2019/day4/solution1 ()
  (loop with total = 0
        with (min max) = (compute-range-limits +aoc2019/day4/input+)
        for n from min to max
        when (and (two-adj-digits-are-the-same n)
                  (digits-never-decrease n))
          count total))

(defun two-adj-digits-are-the-same-restricted (n)
  (loop for s in (streaks (coerce (format nil "~a" n) 'list) '() '())
        thereis (= (length s) 2)))

(defun aoc2019/day4/solution2 ()
  (loop with total = 0
        with (min max) = (compute-range-limits +aoc2019/day4/input+)
        for n from min to max
        when (and (two-adj-digits-are-the-same-restricted n)
                  (digits-never-decrease n))
          count total))
