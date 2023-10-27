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

(define-test aoc2018/day5 :parent aoc2018
  (skip "Slow"
    (is = 11946 (advent-of-code:aoc2018/day5/solution1)))
  (skip "Slow"
    (is = 4248 (advent-of-code:aoc2018/day5/solution2))))

(define-test aoc2018/day7 :parent aoc2018
  (is string= "FMOXCDGJRAUIHKNYZTESWLPBQV"
      (advent-of-code:aoc2018/day7/solution1))
  (is = 1053 (advent-of-code:aoc2018/day7/solution2)))
