(in-package #:advent-of-code/test)

(define-test aoc2023)

(define-test aoc2023/day1 :parent aoc2023
  (is = 55108 (advent-of-code:aoc2023/day1/solution1))
  (is = 56324 (advent-of-code:aoc2023/day1/solution2)))

(define-test aoc2023/day2 :parent aoc2023
  (is = 2563 (advent-of-code:aoc2023/day2/solution1))
  (is = 70768 (advent-of-code:aoc2023/day2/solution2)))

(define-test aoc2023/day3 :parent aoc2023
  (is = 514969 (advent-of-code:aoc2023/day3/solution1))
  (is = 78915902 (advent-of-code:aoc2023/day3/solution2)))

(define-test aoc2023/day4 :parent aoc2023
  (is = 27845 (advent-of-code:aoc2023/day4/solution1))
  (is = 9496801 (advent-of-code:aoc2023/day4/solution2)))

(define-test aoc2023/day5 :parent aoc2023
  (is = 51752125 (advent-of-code:aoc2023/day5/solution1)))

(define-test aoc2023/day6 :parent aoc2023
  (is = 1108800 (advent-of-code:aoc2023/day6/solution1))
  (is = 36919753 (advent-of-code:aoc2023/day6/solution2)))

(define-test aoc2023/day7 :parent aoc2023
  (is = 248453531 (advent-of-code:aoc2023/day7/solution1)))

(define-test aoc2023/day8 :parent aoc2023
  (is = 12169 (advent-of-code:aoc2023/day8/solution1)))

(define-test aoc2023/day9 :parent aoc2023
  (is = 2075724761 (advent-of-code:aoc2023/day9/solution1))
  (is = 1072 (advent-of-code:aoc2023/day9/solution2)))

(define-test aoc2023/day11 :parent aoc2023
  (is = 10885634 (advent-of-code:aoc2023/day11/solution1))
  (is = 707505470642 (advent-of-code:aoc2023/day11/solution2)))
