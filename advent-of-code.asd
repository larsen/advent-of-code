(asdf:defsystem #:advent-of-code
  :description "Describe advent-of-code here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria #:cl-ppcre #:md5 #:memoize #:split-sequence
                            #:jonathan #:magicl
                            #:sketch)
  :components ((:file "package")
               (:file "utils")
               (:module "2015"
                :components ((:file "day1")
                             (:file "day2")
                             (:file "day3")
                             (:file "day4")
                             (:file "day5")
                             (:file "day6")
                             (:file "day7")
                             (:file "day8")
                             (:file "day9")
                             (:file "day10")
                             (:file "day11")
                             (:file "day12")
                             (:file "day13")
                             (:file "day14")
                             (:file "day15")
                             (:file "day16")
                             (:file "day17")
                             (:file "day18")
                             (:file "day19")
                             (:file "day20")
                             (:file "day21")))
               (:module "2016"
                :components ((:file "day1")
                             (:file "day2")
                             (:file "day3")
                             (:file "day4")
                             (:file "day5")
                             (:file "day6")
                             (:file "day7")
                             (:file "day8")
                             (:file "day9")))
               (:module "2017"
                :components ((:file "day1")
                             (:file "day2")
                             (:file "day3")
                             (:file "day4")
                             (:file "day5")
                             (:file "day6")
                             (:file "day7")))
               (:module "2018"
                :components ((:file "day1")
                             (:file "day2")
                             (:file "day3")
                             (:file "day5")
                             (:file "day7")))
               (:module "2019"
                :components ((:file "intcode")
                             (:file "day1")
                             (:file "day2")
                             (:file "day3")
                             (:file "day4")
                             (:file "day5")
                             (:file "day7")))
               (:module "2020"
                :components ((:file "day1")
                             (:file "day2")
                             (:file "day3")
                             (:file "day4")
                             (:file "day5")
                             (:file "day6")
                             (:file "day7")
                             (:file "day8")
                             (:file "day9")
                             (:file "day10")
                             (:file "day11")
                             (:file "day12")
                             (:file "day13")
                             (:file "day14")
                             (:file "day15")
                             (:file "day16")))
               (:module "2021"
                :components ((:file "day1")
                             (:file "day2")
                             (:file "day3")
                             (:file "day4")
                             (:file "day5")
                             (:file "day6")
                             (:file "day7")
                             (:file "day8")
                             (:file "day9")
                             (:file "day10")
                             (:file "day11")
                             (:file "day12")
                             (:file "day13")
                             (:file "day14")
                             (:file "day15")))
               (:module "2022"
                :components ((:file "day1")
                             (:file "day2")
                             (:file "day3")
                             (:file "day4")
                             (:file "day5")
                             (:file "day6")
                             (:file "day7")
                             (:file "day8")
                             (:file "day9")
                             (:file "day10")
                             (:file "day11")
                             (:file "day12")
                             (:file "day13")
                             (:file "day14")
                             (:file "day15")
                             (:file "day16")))
               (:module "2023"
                :components ((:file "day1")
                             (:file "day2"))))
  :in-order-to ((test-op (test-op #:advent-of-code/test))))

(asdf:defsystem #:advent-of-code/test
  :depends-on (#:parachute
               #:advent-of-code)
  :components ((:module "tests"
                :components ((:file "package")
                             (:file "report")
                             (:file "2015")
                             (:file "2016")
                             (:file "2017")
                             (:file "2018")
                             (:file "2019")
                             (:file "2020")
                             (:file "2021")
                             (:file "2022")
                             (:file "2023"))))
  :perform (test-op (op _) (symbol-call :advent-of-code/test :run-aoc-tests)))
