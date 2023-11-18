(defpackage #:advent-of-code
  (:use #:cl #:alexandria
        #:cl-ppcre
        #:md5
        #:org.tfeb.hax.memoize
        #:split-sequence
        #:sketch)
  (:shadowing-import-from #:alexandria
                          #:rotate)
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

           aoc2017/day1/solution1 aoc2017/day1/solution2
           aoc2017/day2/solution1 aoc2017/day2/solution2
           aoc2017/day3/solution1 aoc2017/day3/solution2
           aoc2017/day4/solution1 aoc2017/day4/solution2
           aoc2017/day5/solution1 aoc2017/day5/solution2
           aoc2017/day6/solution1 aoc2017/day6/solution2
           aoc2017/day7/solution1

           aoc2018/day1/solution1 aoc2018/day1/solution2
           aoc2018/day2/solution1 aoc2018/day2/solution2
           aoc2018/day3/solution1 aoc2018/day3/solution2
           aoc2018/day5/solution1 aoc2018/day5/solution2
           aoc2018/day7/solution1 aoc2018/day7/solution2

           aoc2019/day1/solution1 aoc2019/day1/solution2
           aoc2019/day2/solution1 aoc2019/day2/solution2
           aoc2019/day3/solution1 aoc2019/day3/solution2
           aoc2019/day4/solution1 aoc2019/day4/solution2
           aoc2019/day5/solution1 aoc2019/day5/solution2
           aoc2019/day6/solution1 aoc2019/day6/solution2
           aoc2019/day7/solution1 aoc2019/day7/solution2
           aoc2019/day8/solution1 aoc2019/day8/solution2
           aoc2019/day9/solution1 aoc2019/day9/solution2

           aoc2020/day1/solution1 aoc2020/day1/solution2
           aoc2020/day2/solution1 aoc2020/day2/solution2
           aoc2020/day3/solution1 aoc2020/day3/solution2
           aoc2020/day4/solution1 aoc2020/day4/solution2
           aoc2020/day5/solution1 aoc2020/day5/solution2
           aoc2020/day6/solution1 aoc2020/day6/solution2
           aoc2020/day7/solution1 aoc2020/day7/solution2
           aoc2020/day8/solution1 aoc2020/day8/solution2
           aoc2020/day9/solution1 aoc2020/day9/solution2
           aoc2020/day10/solution1 aoc2020/day10/solution2
           aoc2020/day11/solution1 aoc2020/day11/solution2
           aoc2020/day12/solution1 aoc2020/day12/solution2
           aoc2020/day13/solution1 aoc2020/day13/solution2
           aoc2020/day14/solution1 aoc2020/day14/solution2
           aoc2020/day15/solution1 aoc2020/day15/solution2
           aoc2020/day16/solution1

           aoc2021/day1/solution1 aoc2021/day1/solution2
           aoc2021/day2/solution1 aoc2021/day2/solution2
           aoc2021/day3/solution1 aoc2021/day3/solution2
           aoc2021/day4/solution1 aoc2021/day4/solution2
           aoc2021/day5/solution1 aoc2021/day5/solution2
           aoc2021/day6/solution1 aoc2021/day6/solution2
           aoc2021/day7/solution1 aoc2021/day7/solution2
           aoc2021/day8/solution1 aoc2021/day8/solution2
           aoc2021/day9/solution1 aoc2021/day9/solution2
           aoc2021/day10/solution1 aoc2021/day10/solution2
           aoc2021/day11/solution1 aoc2021/day11/solution2
           aoc2021/day12/solution1 aoc2021/day12/solution2
           aoc2021/day12/solution1-iterative
           aoc2021/day13/solution1 aoc2021/day13/solution2
           aoc2021/day14/solution1 aoc2021/day14/solution2
           aoc2021/day15/solution1 aoc2021/day15/solution2

           aoc2022/day1/solution1 aoc2022/day1/solution2
           aoc2022/day2/solution1 aoc2022/day2/solution2
           aoc2022/day3/solution1 aoc2022/day3/solution2
           aoc2022/day4/solution1 aoc2022/day4/solution2
           aoc2022/day5/solution1 aoc2022/day5/solution2
           aoc2022/day6/solution1 aoc2022/day6/solution2
           aoc2022/day7/solution1 aoc2022/day7/solution2
           aoc2022/day8/solution1 aoc2022/day8/solution2
           aoc2022/day9/solution1 aoc2022/day9/solution2
           aoc2022/day10/solution1 aoc2022/day10/solution2
           aoc2022/day11/solution1 aoc2022/day11/solution2
           aoc2022/day12/solution1 aoc2022/day12/solution2
           aoc2022/day13/solution1 aoc2022/day13/solution2
           aoc2022/day14/solution1 aoc2022/day14/solution2
           aoc2022/day15/solution1 aoc2022/day15/solution2
           aoc2022/day16/solution1))
