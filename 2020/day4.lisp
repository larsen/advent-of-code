(in-package #:advent-of-code)

(defun read-passports ()
  (split "\\n\\n" (uiop:read-file-string
                   (asdf:system-relative-pathname 'advent-of-code "inputs/2020/day4"))))

(defun is-passport-semivalid-p (passport)
  (let ((fields-in-passport (loop for field in (split "\\s" passport)
                                  collect (first (split ":" field)))))
    (every (lambda (f)
             (find f fields-in-passport :test #'string=))
           '("byr" "iyr" "eyr" "hgt"
             "hcl" "ecl" "pid"))))

(defun aoc2020/day4/solution1 ()
  (count-if #'is-passport-semivalid-p (read-passports)))

(defun valid-byr (byr-str)
  (let ((byr (parse-integer byr-str)))
    (and (>= byr 1920) (<= byr 2002))))

(defun valid-iyr (iyr-str)
  (let ((iyr (parse-integer iyr-str)))
    (and (>= iyr 2010) (<= iyr 2020))))

(defun valid-eyr (eyr-str)
  (let ((eyr (parse-integer eyr-str)))
    (and (>= eyr 2020) (<= eyr 2030))))

(defun valid-hgt (hgt-str)
  (register-groups-bind ((#'parse-integer height) unit)
      ("\(\\d+\)\(..\)" hgt-str)
    (if (string= "in" unit)
        (and (>= height 59) (<= height 76))
        (and (>= height 150) (<= height 193)))))

(defun valid-hcl (hcl-str)
  (scan "^#[a-f0-9]{6}$" hcl-str))

(defun valid-ecl (ecl-str)
  (find ecl-str '("amb" "blu" "brn" "gry" "grn" "hzl" "oth") :test #'string=))

(defun valid-pid (pid-str)
  (scan "^\\d{9}$" pid-str))

(defun is-passport-valid-p (passport-str)
  (let ((passport (make-hash-table :test 'equal)))
    (loop for (key . value) in (mapcar (lambda (f)
                                         (split ":" f))
                                       (split "\\s" passport-str))
          do (setf (gethash key passport) (first value)))
    (and (is-passport-semivalid-p passport-str)
         (valid-byr (gethash "byr" passport))
         (valid-iyr (gethash "iyr" passport))
         (valid-eyr (gethash "eyr" passport))
         (valid-hgt (gethash "hgt" passport))
         (valid-hcl (gethash "hcl" passport))
         (valid-ecl (gethash "ecl" passport))
         (valid-pid (gethash "pid" passport))
     )))

(defun aoc2020/day4/solution2 ()
  (count-if #'is-passport-valid-p (read-passports)))
