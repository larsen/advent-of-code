(in-package #:advent-of-code/test)

(define-test aoc2017)

(define-test aoc2017/day1 :parent aoc2017
  (is = 1049 (advent-of-code:aoc2017/day1/solution1))
  (is = 1508 (advent-of-code:aoc2017/day1/solution2)))

(define-test aoc2017/day2 :parent aoc2017
  (is = 32121 (advent-of-code:aoc2017/day2/solution1))
  (is = 197 (advent-of-code:aoc2017/day2/solution2)))

(define-test aoc2017/day3 :parent aoc2017
  (is = 480 (advent-of-code:aoc2017/day3/solution1))
  (is = 349975 (advent-of-code:aoc2017/day3/solution2)))

(define-test aoc2017/day4 :parent aoc2017
  (is = 383 (advent-of-code:aoc2017/day4/solution1))
  (is = 265 (advent-of-code:aoc2017/day4/solution2)))

(define-test aoc2017/day5 :parent aoc2017
  (is = 388611 (advent-of-code:aoc2017/day5/solution1))
  (is = 27763113 (advent-of-code:aoc2017/day5/solution2)))

(define-test aoc2017/day6 :parent aoc2017
  (is = 5042 (advent-of-code:aoc2017/day6/solution1))
  (is = 1086 (advent-of-code:aoc2017/day6/solution2)))

(define-test aoc2017/day7 :parent aoc2017
  (is string= "dgoocsw" (advent-of-code:aoc2017/day7/solution1)))

(define-test aoc2017/day8 :parent aoc2017
  (is = 3880 (advent-of-code:aoc2017/day8/solution1))
  (is = 5035 (advent-of-code:aoc2017/day8/solution2)))
