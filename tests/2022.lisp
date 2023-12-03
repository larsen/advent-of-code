(in-package #:advent-of-code/test)

(define-test aoc2022)

(define-test day1 :parent aoc2022
  (is = 69693 (advent-of-code:aoc2022/day1/solution1))
  (is = 200945 (advent-of-code:aoc2022/day1/solution2)))

(define-test day2 :parent aoc2022
  (is = 11386 (advent-of-code:aoc2022/day2/solution1))
  (is = 13600 (advent-of-code:aoc2022/day2/solution2)))

(define-test day3 :parent aoc2022
  (is = 7817 (advent-of-code:aoc2022/day3/solution1))
  (is = 2444 (advent-of-code:aoc2022/day3/solution2)))

(define-test day4 :parent aoc2022
  (is = 573 (advent-of-code:aoc2022/day4/solution1))
  (is = 867 (advent-of-code:aoc2022/day4/solution2)))

(define-test day5 :parent aoc2022
  (is string= "WHTLRMZRC" (advent-of-code:aoc2022/day5/solution1))
  (is string= "GMPMLWNMG" (advent-of-code:aoc2022/day5/solution2)))

(define-test day6 :parent aoc2022
  (is = 1658 (advent-of-code:aoc2022/day6/solution1))
  (is = 2260 (advent-of-code:aoc2022/day6/solution2)))

(define-test day7 :parent aoc2022
  (is = 1315285
      (advent-of-code:aoc2022/day7/solution1))
  (is = 9847279 (advent-of-code:aoc2022/day7/solution2)))

(define-test day8 :parent aoc2022
  (is = 1825 (advent-of-code:aoc2022/day8/solution1))
  (is = 235200 (advent-of-code:aoc2022/day8/solution2)))

(define-test day9 :parent aoc2022
  (is = 5874 (advent-of-code:aoc2022/day9/solution1))
  (is = 2467 (advent-of-code:aoc2022/day9/solution2)))

(define-test day10 :parent aoc2022
  (is = 14860 (advent-of-code:aoc2022/day10/solution1))
  (is string= "RGZEHURK" (advent-of-code:aoc2022/day10/solution2)))

(define-test day11 :parent aoc2022
  (is = 50830 (advent-of-code:aoc2022/day11/solution1))
  (is = 14399640002 (advent-of-code:aoc2022/day11/solution2)))

(define-test day12 :parent aoc2022
  (is = 330 (advent-of-code:aoc2022/day12/solution1))
  (is = 321 (advent-of-code:aoc2022/day12/solution2)))

(define-test day13 :parent aoc2022
  (is = 5366 (advent-of-code:aoc2022/day13/solution1))
  (is = 23391 (advent-of-code:aoc2022/day13/solution2)))

(define-test day14 :parent aoc2022
  (is = 1133 (advent-of-code:aoc2022/day14/solution1))
  (is = 27566 (advent-of-code:aoc2022/day14/solution2)))

(define-test day15 :parent aoc2022
  (skip "Slow"
    (is = 5403290 (advent-of-code:aoc2022/day15/solution1)))
  (is = 10291582906626 (advent-of-code:aoc2022/day15/solution2)))

(define-test day16 :parent aoc2022
  (skip "Slow"
    (is = 1767 (advent-of-code:aoc2022/day16/solution1))))

(define-test day18 :parent aoc2022
  (is = 4300 (advent-of-code:aoc2022/day18/solution1)))
