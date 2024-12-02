(in-package #:advent-of-code/test)

(define-test aoc2024)

(define-test aoc2024/day1 :parent aoc2024
  (is = 2344935 (advent-of-code:aoc2024/day1/solution1))
  (is = 27647262 (advent-of-code:aoc2024/day1/solution2)))

(define-test aoc2024/day2 :parent aoc2024
  (is = 314 (advent-of-code:aoc2024/day2/solution1))
  (is = 373 (advent-of-code:aoc2024/day2/solution2)))
