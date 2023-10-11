(in-package #:advent-of-code)

(defun read-crates-config-and-moves ()
  (destructuring-bind (crates-config-raw moves-raw)
      (split-sequence "" (uiop:read-file-lines
                          (asdf:system-relative-pathname :advent-of-code "inputs/2022/day5"))
                      :test 'string=)
    (values
     ;; Parse crates config
     (mapcar (lambda (lst)
               (coerce
                (string-trim " " (coerce lst 'string))
                'list))
             (remove-if (lambda (lst)
                          (or (member #\[ lst)
                              (member #\] lst)
                              (loop for item in lst
                                    always (char= #\Space item))))
                        (transpose (loop for cc in crates-config-raw
                                         collect (coerce (format nil "~35a" cc) 'list)))))
     ;; Parse moves
     (loop for m-raw in moves-raw
           ;; move 1 from 2 to 1
           collect (register-groups-bind ((#'parse-integer qty start end))
                       ("move (\\d+) from (\\d+) to (\\d+)" m-raw)
                     (list qty start end))))))


(defun aoc2022/day5/solution1 ()
  (multiple-value-bind (crane-config moves)
      (read-crates-config-and-moves)
    (loop for (qty start end) in moves
          do (loop with item
                   repeat qty
                   do (setf item (pop (nth (- start 1) crane-config)))
                      (push item (nth (- end 1) crane-config))))
    (coerce (mapcar #'first crane-config)
            'string)))


(defun aoc2022/day5/solution2 ()
  (multiple-value-bind (crane-config moves)
      (read-crates-config-and-moves)
    (loop for (qty start end) in moves
          do (loop with buffer = '()
                   repeat qty
                   do (push (pop (nth (- start 1) crane-config))
                            buffer)
                   finally (dolist (item buffer)
                             (push item (nth (- end 1) crane-config)))))
    (coerce (mapcar #'first crane-config)
            'string)))
