(in-package #:advent-of-code/test)

(define-test aoc2019)

(define-test aoc2019/day1 :parent aoc2019
  (is = 3161483 (advent-of-code:aoc2019/day1/solution1))
  (is = 4739374 (advent-of-code:aoc2019/day1/solution2)))

(define-test aoc2019/day2 :parent aoc2019
  (is = 4714701 (advent-of-code:aoc2019/day2/solution1))
  (is = 5121 (advent-of-code:aoc2019/day2/solution2)))

(define-test aoc2019/day3 :parent aoc2019
  (is = 303 (advent-of-code:aoc2019/day3/solution1))
  (is = 11222 (advent-of-code:aoc2019/day3/solution2)))
