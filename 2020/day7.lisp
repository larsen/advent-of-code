(in-package #:advent-of-code)

(defun read-bag-rules ()
  (let ((graph (make-hash-table :test 'equal)))
    (loop for rule in (uiop:read-file-lines
                       (asdf:system-relative-pathname 'advent-of-code "inputs/2020/day7"))
          do (register-groups-bind (color rest-of-rule)
                 ("^\(\\w+ \\w+\) bags contain \(.*\)" rule)
               ;; (format t "~A: ~A~%" color rest-of-rule)
               (setf (gethash color graph)
                     (loop for rule-part in (split-sequence #\, rest-of-rule)
                           ;; do (format t "~A~%" rule-part)
                           collect (register-groups-bind ((#'parse-integer n) c-color)
                                       ("\(\\d+\) \(\\w+ \\w+\) bag" rule-part)
                                     (cons c-color n))))))
    graph))

(defun path (graph start end)
  (defun path-using (graph start end node-list)
    (if (equal start end)
        (return-from path (car (reverse node-list)))
        (dolist (child (mapcar #'car (gethash start graph)))
          (path-using graph child end (cons child node-list)))))
  (path-using graph start end (list start)))

(defun aoc2020/day7/solution1 ()
  (let ((graph (read-bag-rules))
        (solutions 0))
    (loop for color being the hash-keys of graph
          when (and (not (string= color "shiny gold"))
                    (path graph color "shiny gold"))
            do (incf solutions)
          finally (return solutions))))

(defun total-bags (start bag-rules)
  (+ 1 (loop for (color . quantity) in (gethash start bag-rules)
             when (numberp quantity)
               sum (* quantity (total-bags color bag-rules)))))

(defun aoc2020/day7/solution2 ()
  ;; subtracting 1 because we don't want to count the
  ;; shiny gold bag
  (- (total-bags "shiny gold" (read-bag-rules)) 1))
