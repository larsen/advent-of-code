(asdf:defsystem #:advent-of-code
  :description "Describe advent-of-code here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria #:cl-ppcre #:md5 #:memoize #:split-sequence
                            #:jonathan #:magicl)
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
                             (:file "day21"))))
  :in-order-to ((test-op (test-op #:advent-of-code/test))))

(asdf:defsystem #:advent-of-code/test
  :depends-on (#:parachute
               #:advent-of-code)
  :components ((:module "tests"
                :components ((:file "package")
                             (:file "2015"))))
  :perform (test-op (op _) (symbol-call :parachute :test :advent-of-code/test)))
