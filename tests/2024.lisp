(in-package #:advent-of-code/test)

(define-test aoc2024)

(define-test aoc2024/day1 :parent aoc2024
  (is = 2344935 (advent-of-code:aoc2024/day1/solution1))
  (is = 27647262 (advent-of-code:aoc2024/day1/solution2)))

(define-test aoc2024/day2 :parent aoc2024
  (is = 314 (advent-of-code:aoc2024/day2/solution1))
  (is = 373 (advent-of-code:aoc2024/day2/solution2)))

(define-test aoc2024/day3 :parent aoc2024
  (is = 161289189 (advent-of-code:aoc2024/day3/solution1))
  (is = 83595109 (advent-of-code:aoc2024/day3/solution2)))

(define-test aoc2024/day4 :parent aoc2024
  (is = 2554 (advent-of-code:aoc2024/day4/solution1))
  (is = 1916 (advent-of-code:aoc2024/day4/solution2)))

(define-test aoc2024/day5 :parent aoc2024
  (is = 4872 (advent-of-code:aoc2024/day5/solution1))
  (is = 5564 (advent-of-code:aoc2024/day5/solution2)))

(define-test aoc2024/day6 :parent aoc2024
  (is = 4778 (advent-of-code:aoc2024/day6/solution1)))
