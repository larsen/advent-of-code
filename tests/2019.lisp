(in-package #:advent-of-code/test)

(define-test aoc2019)

(define-test aoc2019/day1 :parent aoc2019
  (is = 3161483 (advent-of-code:aoc2019/day1/solution1))
  (is = 4739374 (advent-of-code:aoc2019/day1/solution2)))

(define-test aoc2019/day2 :parent aoc2019
  (is = 4714701 (advent-of-code:aoc2019/day2/solution1))
  (is = 5121 (advent-of-code:aoc2019/day2/solution2)))

(define-test aoc2019/day3 :parent aoc2019
  (skip "Slow"
    (is = 303 (advent-of-code:aoc2019/day3/solution1)))
  ;(is = 11222 (advent-of-code:aoc2019/day3/solution2))
  )

(define-test aoc2019/day4 :parent aoc2019
  (is = 1063 (advent-of-code:aoc2019/day4/solution1))
  (is = 686 (advent-of-code:aoc2019/day4/solution2)))

(define-test aoc2019/day5 :parent aoc2019
  (is = 8332629 (advent-of-code:aoc2019/day5/solution1))
  (is = 8805067 (advent-of-code:aoc2019/day5/solution2)))

(define-test aoc2019/day6 :parent aoc2019
  (is = 402879 (advent-of-code:aoc2019/day6/solution1))
  (is = 484 (advent-of-code:aoc2019/day6/solution2)))

(define-test aoc2019/day7 :parent aoc2019
  (is = 67023 (advent-of-code:aoc2019/day7/solution1))
  (is = 0 (advent-of-code:aoc2019/day7/solution2)))

(define-test aoc2019/day8 :parent aoc2019
  (is = 2684 (advent-of-code:aoc2019/day8/solution1))
  (is string= "YGRUZ" (advent-of-code:aoc2019/day8/solution2)))

(define-test aoc2019/day9 :parent aoc2019
  (is = 3906448201 (advent-of-code:aoc2019/day9/solution1))
  (is = 59785 (advent-of-code:aoc2019/day9/solution2)))

(define-test aoc2019/day10 :parent aoc2019
  (is = 288 (advent-of-code:aoc2019/day10/solution1)))

(define-test aoc2019/day12 :parent aoc2019
  (is = 5350 (advent-of-code:aoc2019/day12/solution1)))

(define-test aoc2019/day13 :parent aoc2019
  (is = 239 (advent-of-code:aoc2019/day13/solution1)))
