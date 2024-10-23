(in-package #:advent-of-code/test)

(define-test aoc2016)

(define-test aoc2016/day1 :parent aoc2016
  (is = 273 (advent-of-code:aoc2016/day1/solution1))
  (is = 115 (advent-of-code:aoc2016/day1/solution2)))

(define-test aoc2016/day2 :parent aoc2016
  (is string= "76792" (advent-of-code:aoc2016/day2/solution1))
  (is string= "A7AC3" (advent-of-code:aoc2016/day2/solution2)))

(define-test aoc2016/day3 :parent aoc2016
  (is = 1050 (advent-of-code:aoc2016/day3/solution1))
  (is = 1921 (advent-of-code:aoc2016/day3/solution2)))

(define-test aoc2016/day4 :parent aoc2016
  (is = 137896 (advent-of-code:aoc2016/day4/solution1))
  (is = 501 (advent-of-code:aoc2016/day4/solution2)))

;; Slow
(define-test aoc2016/day5 :parent aoc2016
  (skip "Slow"
    (is string= "801b56a7" (advent-of-code:aoc2016/day5/solution1 "abbhdwsy")))
  (skip "Slow"
    (is string= "424a0197" (advent-of-code:aoc2016/day5/solution2 "abbhdwsy"))))

(define-test aoc2016/day6 :parent aoc2016
  (is string= "gyvwpxaz" (advent-of-code:aoc2016/day6/solution1))
  (is string= "jucfoary" (advent-of-code:aoc2016/day6/solution2)))

(define-test aoc2016/day7 :parent aoc2016
  (is = 105 (advent-of-code:aoc2016/day7/solution1))
  (is = 258 (advent-of-code:aoc2016/day7/solution2)))

(define-test aoc2016/day8 :parent aoc2016
  (is = 119 (advent-of-code:aoc2016/day8/solution1))
  (is string= "ZFHFSFOGPO" (advent-of-code:aoc2016/day8/solution2)))

(define-test aoc2016/day9 :parent aoc2016
  ;; (is = 6 (length (decompress "ADVENT")))
  ;; (is = 7 (length (decompress "A(1x5)BC")))
  ;; (is = 9 (length (decompress "(3x3)XYZ")))
  ;; (is = 11 (length (decompress "A(2x2)BCD(2x2)EFG")))
  ;; (is = 6 (length (decompress "(6x1)(1x3)A")))
  ;; (is = 18 (length (decompress "X(8x2)(3x3)ABCY")))
  (is = 150914 (advent-of-code:aoc2016/day9/solution1))
  (is = 0 (advent-of-code:aoc2016/day9/solution2)))

(define-test aoc2016/day12 :parent aoc2016
  (skip "Slow"
    (is = 318007 (advent-of-code:aoc2016/day12/solution1)))
  (skip "Slow"
    (is = 9227661 (advent-of-code:aoc2016/day12/solution2))))
