(in-package #:advent-of-code/test)

(define-test aoc2015)

(define-test aoc2015/day1 :parent aoc2015
  (is = 74 (advent-of-code:aoc2015/day1/solution1))
  (is = 1795 (advent-of-code:aoc2015/day1/solution2)))

(define-test aoc2015/day2 :parent aoc2015
  (is = 1586300 (advent-of-code:aoc2015/day2/solution1))
  (is = 3737498 (advent-of-code:aoc2015/day2/solution2)))

(define-test aoc2015/day3 :parent aoc2015
  (is = 2572 (advent-of-code:aoc2015/day3/solution1))
  (is = 2631 (advent-of-code:aoc2015/day3/solution2)))

(define-test aoc2015/day4 :parent aoc2015
  (is = 346386 (advent-of-code:aoc2015/day4/solution1))
  (skip "Slow"
    (is = 9958218 (advent-of-code:aoc2015/day4/solution2))))

(define-test aoc2015/day5 :parent aoc2015
  (is = (advent-of-code:aoc2015/day5/solution1) 258)
  (is = (advent-of-code:aoc2015/day5/solution2) 53))

(define-test aoc2015/day6 :parent aoc2015
  (is = 569999 (advent-of-code:aoc2015/day6/solution1))
  (is = 17836115 (advent-of-code:aoc2015/day6/solution2)))

(define-test aoc2015/day7 :parent aoc2015
  (is = 16076 (advent-of-code:aoc2015/day7/solution1))
  (is = 2797 (advent-of-code:aoc2015/day7/solution2)))

(define-test aoc2015/day8 :parent aoc2015
  (is = 1342 (advent-of-code:aoc2015/day8/solution1))
  (is = 2074 (advent-of-code:aoc2015/day8/solution2)))

(define-test aoc2015/day9 :parent aoc2015
  (is = 251 (advent-of-code:aoc2015/day9/solution1))
  (is = 898 (advent-of-code:aoc2015/day9/solution2)))

(define-test aoc2015/day10 :parent aoc2015
  (is = 329356 (advent-of-code:aoc2015/day10/solution1))
  (is = 4666278 (advent-of-code:aoc2015/day10/solution2)))

(define-test aoc2015/day11 :parent aoc2015
  (skip "Slow"
    (is string= "hepxxyzz" (advent-of-code:aoc2015/day11/solution1)))
  (skip "Slow"
    (is string= "heqaabcc" (advent-of-code:aoc2015/day11/solution2))))

(define-test aoc2015/day12 :parent aoc2015
  (is = 111754 (advent-of-code:aoc2015/day12/solution1))
  (is = 65402 (advent-of-code:aoc2015/day12/solution2)))

(define-test aoc2015/day13 :parent aoc2015
  (is = 664 (advent-of-code:aoc2015/day13/solution1))
  (is = 640 (advent-of-code:aoc2015/day13/solution2)))

(define-test aoc2015/day14 :parent aoc2015
  (is = 2640 (advent-of-code:aoc2015/day14/solution1))
  (is = 1102 (advent-of-code:aoc2015/day14/solution2)))

(define-test aoc2015/day15 :parent aoc2015
  (is = 222870 (advent-of-code:aoc2015/day15/solution1))
  (is = 117936 (advent-of-code:aoc2015/day15/solution2)))

(define-test aoc2015/day16 :parent aoc2015
  (is = 373 (advent-of-code:aoc2015/day16/solution1))
  (is = 260 (advent-of-code:aoc2015/day16/solution2)))

(define-test aoc2015/day17 :parent aoc2015
  (is = 1304 (advent-of-code:aoc2015/day17/solution1))
  (is = 18 (advent-of-code:aoc2015/day17/solution2)))

(define-test aoc2015/day18 :parent aoc2015
  (is = 1061 (advent-of-code:aoc2015/day18/solution1))
  (is = 1006 (advent-of-code:aoc2015/day18/solution2)))

(define-test aoc2015/day19 :parent aoc2015
  (is = 518 (advent-of-code:aoc2015/day19/solution1)))

;; (define-test aoc2015/day20 :parent aoc2015
;;   (is = 831600 (advent-of-code:aoc2015/day20/solution1)))
