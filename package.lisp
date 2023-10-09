(defpackage #:advent-of-code
  (:use #:cl #:alexandria
        #:cl-ppcre
        #:md5
        #:org.tfeb.hax.memoize
        #:split-sequence)
  (:export aoc2015/day1/solution1 aoc2015/day1/solution2
           aoc2015/day2/solution1 aoc2015/day2/solution2
           aoc2015/day3/solution1 aoc2015/day3/solution2
           aoc2015/day4/solution1 aoc2015/day4/solution2
           aoc2015/day5/solution1 aoc2015/day5/solution2
           aoc2015/day6/solution1 aoc2015/day6/solution2
           aoc2015/day7/solution1 aoc2015/day7/solution2
           aoc2015/day8/solution1 aoc2015/day8/solution2
           aoc2015/day9/solution1 aoc2015/day9/solution2
           aoc2015/day10/solution1 aoc2015/day10/solution2
           aoc2015/day11/solution1 aoc2015/day11/solution2
           aoc2015/day12/solution1 aoc2015/day12/solution2
           aoc2015/day13/solution1 aoc2015/day13/solution2
           aoc2015/day14/solution1 aoc2015/day14/solution2
           aoc2015/day15/solution1 aoc2015/day15/solution2
           aoc2015/day16/solution1 aoc2015/day16/solution2
           aoc2015/day17/solution1 aoc2015/day17/solution2
           aoc2015/day18/solution1 aoc2015/day18/solution2
           aoc2015/day19/solution1 aoc2015/day19/solution2
           aoc2015/day20/solution1 aoc2015/day20/solution2
           aoc2015/day21/solution1 aoc2015/day21/solution2

           aoc2016/day1/solution1 aoc2016/day1/solution2
           aoc2016/day2/solution1 aoc2016/day2/solution2
           aoc2016/day3/solution1 aoc2016/day3/solution2
           aoc2016/day4/solution1 aoc2016/day4/solution2
           aoc2016/day5/solution1 aoc2016/day5/solution2
           aoc2016/day6/solution1 aoc2016/day6/solution2
           aoc2016/day7/solution1 aoc2016/day7/solution2
           aoc2016/day8/solution1 aoc2016/day8/solution2
           decompress
           aoc2016/day9/solution1 aoc2016/day9/solution2
           ))
