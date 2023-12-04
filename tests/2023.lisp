(in-package #:advent-of-code/test)

(define-test aoc2023)

(define-test day1 :parent aoc2023
  (is = 55108 (advent-of-code:aoc2023/day1/solution1))
  (is = 56324 (advent-of-code:aoc2023/day1/solution2)))

(define-test day2 :parent aoc2023
  (is = 2563 (advent-of-code:aoc2023/day2/solution1))
  (is = 70768 (advent-of-code:aoc2023/day2/solution2)))

(define-test day3 :parent aoc2023
  (is = 514969 (advent-of-code:aoc2023/day3/solution1))
  (is = 78915902 (advent-of-code:aoc2023/day3/solution2)))

(define-test day4 :parent aoc2023
  (is = 27845 (advent-of-code:aoc2023/day4/solution1))
  (is = 9496801 (advent-of-code:aoc2023/day4/solution2)))
