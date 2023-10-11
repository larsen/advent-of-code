(in-package #:advent-of-code/test)

(define-test aoc2021)

(define-test aoc2021/day1 :parent aoc2021
  (is = 1400 (advent-of-code:aoc2021/day1/solution1))
  (is = 1429 (advent-of-code:aoc2021/day1/solution2)))

(define-test aoc2021/day2 :parent aoc2021
  (is = 2019945 (advent-of-code:aoc2021/day2/solution1))
  (is = 1599311480 (advent-of-code:aoc2021/day2/solution2)))

(define-test aoc2021/day3 :parent aoc2021
  (is = 852500 (advent-of-code:aoc2021/day3/solution1))
  (is = 1007985 (advent-of-code:aoc2021/day3/solution2)))

(define-test aoc2021/day4 :parent aoc2021
  (is = 71708 (advent-of-code:aoc2021/day4/solution1))
  (is = 34726 (advent-of-code:aoc2021/day4/solution2)))

(define-test aoc2021/day5 :parent aoc2021
  (is = 6710 (advent-of-code:aoc2021/day5/solution1))
  (is = 20121 (advent-of-code:aoc2021/day5/solution2)))

(define-test aoc2021/day6 :parent aoc2021
  (is = 386755 (advent-of-code:aoc2021/day6/solution1))
  (is = 1732731810807 (advent-of-code:aoc2021/day6/solution2)))

(define-test aoc2021/day7 :parent aoc2021
  (is = 356922 (advent-of-code:aoc2021/day7/solution1))
  (is = 100347031 (advent-of-code:aoc2021/day7/solution2)))

(define-test aoc2021/day8 :parent aoc2021
  (is = 440 (advent-of-code:aoc2021/day8/solution1))
  (is = 1046281 (advent-of-code:aoc2021/day8/solution2)))

(define-test aoc2021/day9 :parent aoc2021
  (is = 560 (advent-of-code:aoc2021/day9/solution1))
  (is = 959136 (advent-of-code:aoc2021/day9/solution2)))

(define-test aoc2021/day10 :parent aoc2021
  (is = 370407 (advent-of-code:aoc2021/day10/solution1))
  (is = 3249889609 (advent-of-code:aoc2021/day10/solution2)))

(define-test aoc2021/day11 :parent aoc2021
  (is = 1757 (advent-of-code:aoc2021/day11/solution1))
  (is = 422 (advent-of-code:aoc2021/day11/solution2)))

(define-test aoc2021/day12 :parent aoc2021
  (is = 4707 (advent-of-code:aoc2021/day12/solution1))
  (is = (advent-of-code:aoc2021/day12/solution1-iterative)
      (advent-of-code:aoc2021/day12/solution1))
  (is = 130493 (advent-of-code:aoc2021/day12/solution2)))

(define-test aoc2021/day13 :parent aoc2021
  (is = 693 (advent-of-code:aoc2021/day13/solution1))
  (is string= "UCLZRAZU" (advent-of-code:aoc2021/day13/solution2)))
