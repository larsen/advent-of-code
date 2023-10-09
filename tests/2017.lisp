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
  ;(is (= (advent-of-code:aoc2017/day3/solution2) 0))
  )

(define-test aoc2017/day4 :parent aoc2017
  (is = 383 (advent-of-code:aoc2017/day4/solution1))
  (is = 265 (advent-of-code:aoc2017/day4/solution2)))

(define-test aoc2017/day5 :parent aoc2017
  (is = 388611 (advent-of-code:aoc2017/day5/solution1))
  (is = 27763113 (advent-of-code:aoc2017/day5/solution2)))
