(in-package #:advent-of-code/test)

(define-test aoc2020)

(define-test aoc2020/day1 :parent aoc2020
  (is = 970816 (advent-of-code:aoc2020/day1/solution1))
  (is = 96047280 (advent-of-code:aoc2020/day1/solution2)))

(define-test aoc2020/day2 :parent aoc2020
  (is = 542 (advent-of-code:aoc2020/day2/solution1))
  (is = 360 (advent-of-code:aoc2020/day2/solution2)))

(define-test aoc2020/day3 :parent aoc2020
  (is = 286 (advent-of-code:aoc2020/day3/solution1))
  (is = 3638606400 (advent-of-code:aoc2020/day3/solution2)))

(define-test aoc2020/day4 :parent aoc2020
  (is = 210 (advent-of-code:aoc2020/day4/solution1))
  (is = 131 (advent-of-code:aoc2020/day4/solution2)))

(define-test aoc2020/day5 :parent aoc2020
  (is = 987 (advent-of-code:aoc2020/day5/solution1))
  (is = 603 (advent-of-code:aoc2020/day5/solution2)))

(define-test aoc2020/day6 :parent aoc2020
  (is = 6742 (advent-of-code:aoc2020/day6/solution1))
  (is = 3447 (advent-of-code:aoc2020/day6/solution2)))

(define-test aoc2020/day7 :parent aoc2020
  (is = 169 (advent-of-code:aoc2020/day7/solution1))
  (is = 82372 (advent-of-code:aoc2020/day7/solution2)))

(define-test aoc2020/day8 :parent aoc2020
  (is = 1584 (advent-of-code:aoc2020/day8/solution1))
  (is = 920 (advent-of-code:aoc2020/day8/solution2)))

(define-test aoc2020/day9 :parent aoc2020
  (is = 22477624 (advent-of-code:aoc2020/day9/solution1))
  (is = 2980044 (advent-of-code:aoc2020/day9/solution2)))

(define-test aoc2020/day10 :parent aoc2020
  (is = 3000 (advent-of-code:aoc2020/day10/solution1))
  (is = 193434623148032 (advent-of-code:aoc2020/day10/solution2)))

(define-test aoc2020/day11 :parent aoc2020
  (is = 2238 (advent-of-code:aoc2020/day11/solution1))
  (is = 2013 (advent-of-code:aoc2020/day11/solution2)))

(define-test aoc2020/day12 :parent aoc2020
  (is = 1007 (advent-of-code:aoc2020/day12/solution1))
  (is = 41212 (advent-of-code:aoc2020/day12/solution2)))

(define-test aoc2020/day13 :parent aoc2020
  (is = 3606 (advent-of-code:aoc2020/day13/solution1))
  (is = 379786358533423 (advent-of-code:aoc2020/day13/solution2)))

(define-test aoc2020/day14 :parent aoc2020
  (is = 9879607673316 (advent-of-code:aoc2020/day14/solution1))
  (is = 3435342392262 (advent-of-code:aoc2020/day14/solution2)))

(define-test aoc2020/day15 :parent aoc2020
  (is = 387 (advent-of-code:aoc2020/day15/solution1))
  (skip "Slow"
    (is = 6428 (advent-of-code:aoc2020/day15/solution2))))

(define-test aoc2020/day16 :parent aoc2020
  (is = 20048 (advent-of-code:aoc2020/day16/solution1)))
