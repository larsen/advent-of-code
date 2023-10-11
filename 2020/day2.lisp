(in-package #:advent-of-code)

(defun read-passwords-and-policies ()
  (let ((lines (uiop:read-file-lines
                (asdf:system-relative-pathname 'advent-of-code "inputs/2020/day2"))))
    (loop for l in lines
          collect (register-groups-bind ((#'parse-integer min-occ max-occ)
                                         (#'character letter)
                                         password)
                      ("^\(\\d+\)-\(\\d+\) \(\\w\): \(\\w+\)$" l)
                    (list letter min-occ max-occ password)))))

(defun password-valid-p (letter min-occ max-occ password)
  (let ((occurrencies (count letter password)))
    (and (>= occurrencies min-occ)
         (<= occurrencies max-occ))))

(defun aoc2020/day2/solution1 ()
  (let ((passwords-and-policies (read-passwords-and-policies)))
    (count-if (lambda (lst)
                (apply #'password-valid-p lst))
              passwords-and-policies)))

(defun password-valid-positional-p (letter pos1 pos2 password)
  (let ((password-as-list (coerce password 'list)))
    (alexandria:xor (char= letter (nth (- pos1 1) password-as-list))
                    (char= letter (nth (- pos2 1) password-as-list)))))

(defun aoc2020/day2/solution2 ()
  (let ((passwords-and-policies (read-passwords-and-policies)))
    (count-if (lambda (lst)
                (apply #'password-valid-positional-p lst))
              passwords-and-policies)))
