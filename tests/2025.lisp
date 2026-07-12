(in-package #:advent-of-code/test)

(define-test aoc2025)

(define-test aoc2025/day1 :parent aoc2025
  (is = 1123 (advent-of-code:aoc2025/day1/solution1)))

(define-test aoc2025/day3 :parent aoc2025
  (is = 17332 (advent-of-code:aoc2025/day3/solution1)))

(define-test aoc2025/day4 :parent aoc2025
  (is = 1416 (advent-of-code:aoc2025/day4/solution1))
  (is = 9096 (advent-of-code:aoc2025/day4/solution2)))

(define-test aoc2025/day5 :parent aoc2025
  (is = 720 (advent-of-code:aoc2025/day5/solution1)))

(define-test aoc2025/day6 :parent aoc2025
  (is = 5873191732773 (advent-of-code:aoc2025/day6/solution1)))
