(in-package #:advent-of-code/test)

(define-test aoc2018)

(define-test aoc2018/day1 :parent aoc2018
  (is = (advent-of-code:aoc2018/day1/solution1) 582)
  (is = (advent-of-code:aoc2018/day1/solution2) 488))

(define-test aoc2018/day2 :parent aoc2018
  (is = (advent-of-code:aoc2018/day2/solution1) 5390)
  (is string= (advent-of-code:aoc2018/day2/solution2)
      "nvosmkcdtdbfhyxsphzgraljq"))

(define-test aoc2018/day3 :parent aoc2018
  (is = (advent-of-code:aoc2018/day3/solution1) 116489)
  (skip "Slow"
    (is = (advent-of-code:aoc2018/day3/solution2) 1260)))
