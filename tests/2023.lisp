(in-package #:advent-of-code/test)

(define-test aoc2023)

(define-test day1 :parent aoc2023
  (is = 55108 (advent-of-code:aoc2023/day1/solution1))
  (is = 56324 (advent-of-code:aoc2023/day1/solution2)))

(define-test day2 :parent aoc2023
  (is = 2563 (advent-of-code:aoc2023/day2/solution1))
  (is = 70768 (advent-of-code:aoc2023/day2/solution2)))

(define-test day3 :parent aoc2023
  (is = 514969 (advent-of-code:aoc2023/day3/solution1))
  (is = 78915902 (advent-of-code:aoc2023/day3/solution2)))

(define-test day4 :parent aoc2023
  (is = 27845 (advent-of-code:aoc2023/day4/solution1))
  (is = 9496801 (advent-of-code:aoc2023/day4/solution2)))

(define-test day5 :parent aoc2023
  (is = 51752125 (advent-of-code:aoc2023/day5/solution1)))

(define-test day6 :parent aoc2023
  (is = 1108800 (advent-of-code:aoc2023/day6/solution1))
  (is = 36919753 (advent-of-code:aoc2023/day6/solution2)))

(define-test day7 :parent aoc2023
  (is = 248453531 (advent-of-code:aoc2023/day7/solution1)))
