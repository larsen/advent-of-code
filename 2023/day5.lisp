(in-package #:advent-of-code)

(defun chunks (lst n)
  (loop for idx from 0 below (length lst) by n
        collect (subseq lst idx (alexandria:clamp (+ idx n) 0 (length lst)))))

(defun read-almanac ()
  (destructuring-bind (seeds
                       seed-to-soil
                       soil-to-fertilizer
                       fertilizer-to-water
                       water-to-light
                       light-to-temperature
                       temperature-to-humidity
                       humidity-to-location)
      (split "\\n\\n"
             (uiop:read-file-string (asdf:system-relative-pathname
                                     'advent-of-code "inputs/2023/day5")))
    (values (mapcar #'parse-integer (all-matches-as-strings "\\d+" seeds))
            (chunks (mapcar #'parse-integer
                            (all-matches-as-strings "\\d+" seed-to-soil)) 3)
            (chunks (mapcar #'parse-integer
                            (all-matches-as-strings "\\d+" soil-to-fertilizer)) 3)
            (chunks (mapcar #'parse-integer
                            (all-matches-as-strings "\\d+" fertilizer-to-water)) 3)
            (chunks (mapcar #'parse-integer
                            (all-matches-as-strings "\\d+" water-to-light)) 3)
            (chunks (mapcar #'parse-integer
                            (all-matches-as-strings "\\d+" light-to-temperature)) 3)
            (chunks (mapcar #'parse-integer
                            (all-matches-as-strings "\\d+" temperature-to-humidity)) 3)
            (chunks (mapcar #'parse-integer
                            (all-matches-as-strings "\\d+" humidity-to-location)) 3))))

(defun apply-mappings (mappings n)
  (loop for (destination-range-start source-range-start range-size) in mappings
        when (<= source-range-start n (+ source-range-start range-size -1))
          do (return (+ destination-range-start (- n source-range-start)))
        finally (return n)))

(defun aoc2023/day5/solution1 ()
  (multiple-value-bind (seeds
                       seed-to-soil
                       soil-to-fertilizer
                       fertilizer-to-water
                       water-to-light
                       light-to-temperature
                       temperature-to-humidity
                       humidity-to-location)
      (read-almanac)
    (loop with apply-all-mappings = (alexandria:compose
                                     (serapeum:partial #'apply-mappings humidity-to-location)
                                     (serapeum:partial #'apply-mappings temperature-to-humidity)
                                     (serapeum:partial #'apply-mappings light-to-temperature)
                                     (serapeum:partial #'apply-mappings water-to-light)
                                     (serapeum:partial #'apply-mappings fertilizer-to-water)
                                     (serapeum:partial #'apply-mappings soil-to-fertilizer)
                                     (serapeum:partial #'apply-mappings seed-to-soil))
          for s in seeds
          minimizing (funcall apply-all-mappings s))))

(defun apply-mappings-on-range (mappings range)
  (loop with (range-start range-size) = range
        for (destination-range-start source-range-start mapping-range-size) in mappings
        for source-range-end = (+ source-range-start mapping-range-size -1)
        when (<= source-range-start range-start source-range-end)
          do (return (list
                      ;; nuovo range
                      (list (+ destination-range-start (- range-start source-range-start))
                            (if (>= (- source-range-end range-start) range-size)
                                range-size
                                (- range-size (- source-range-end range-start))))
                      ;; eventuale resto
                      (if (>= (- source-range-end range-start) range-size)
                                nil  ; no resto
                                (list
                                 (- ) (- range-size (- source-range-end range-start))))
                      ))
        finally (return range)))

(defun aoc2023/day5/solution2 ()
  (multiple-value-bind (seeds
                        seed-to-soil
                        soil-to-fertilizer
                        fertilizer-to-water
                        water-to-light
                        light-to-temperature
                        temperature-to-humidity
                        humidity-to-location)
      (read-almanac)
    (loop for (start size) in (chunks seeds 2)
          )))
