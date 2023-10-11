(in-package #:advent-of-code/test)

(define-test aoc2018)

(define-test aoc2018/day1 :parent aoc2018
  (is = 582 (advent-of-code:aoc2018/day1/solution1))
  (is = 488 (advent-of-code:aoc2018/day1/solution2)))

(define-test aoc2018/day2 :parent aoc2018
  (is = 5390 (advent-of-code:aoc2018/day2/solution1))
  (is string= "nvosmkcdtdbfhyxsphzgraljq"
      (advent-of-code:aoc2018/day2/solution2)))

(define-test aoc2018/day3 :parent aoc2018
  (is = 116489 (advent-of-code:aoc2018/day3/solution1))
  (skip "Slow"
    (is = 1260 (advent-of-code:aoc2018/day3/solution2))))
