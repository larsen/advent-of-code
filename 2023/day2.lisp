(in-package #:advent-of-code)

(defun parse-game (str)
  (register-groups-bind ((#'parse-integer id) reveals)
      ("Game (\\d+): (.*)" str)
    (list id
          (mapcar (lambda (str)
                    (let ((r 0)
                          (g 0)
                          (b 0))
                      (loop for nc in (split ", " str)
                            for (n-as-str color) = (split " " nc)
                            for n = (parse-integer n-as-str)
                            do (cond
                                 ((string= color "red") (setf r n))
                                 ((string= color "green") (setf g n))
                                 ((string= color "blue") (setf b n))))
                      (list r g b)))
                  (split "; " reveals)))))

(defun read-games ()
  (loop for line in (uiop:read-file-lines (asdf:system-relative-pathname
                                           'advent-of-code
                                           "inputs/2023/day2"))
        collect (parse-game line)))

(defun is-game-possible-p (reveals)
  (loop for (r g b) in reveals
        always (and (<= r 12)
                    (<= g 13)
                    (<= b 14))))

(defun aoc2023/day2/solution1 ()
  (loop for (id reveals) in (read-games)
        when (is-game-possible-p reveals)
          sum id))

(defun min-rgb-power (reveals)
  (loop for (r g b) in reveals
        maximizing r into max-r
        maximizing g into max-g
        maximizing b into max-b
        finally (return (* max-r max-g max-b))))

(defun aoc2023/day2/solution2 ()
  (loop for (id reveals) in (read-games)
        sum (min-rgb-power reveals)))
