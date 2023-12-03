(in-package #:advent-of-code/test)

(define-test aoc2023)

(define-test day1 :parent aoc2023
  (is = 55108 (advent-of-code:aoc2023/day1/solution1))
  (is = 56324 (advent-of-code:aoc2023/day1/solution2)))

(define-test day2 :parent aoc2023
  (is = 2563 (advent-of-code:aoc2023/day2/solution1))
  (is = 70768 (advent-of-code:aoc2023/day2/solution2)))
