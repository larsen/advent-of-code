(in-package #:advent-of-code/test)

;; To run tests for a single year and print a report
;; from the advent-of-code/test package:
;; (test 'aoc2024 :report 'aoc-report)

(defclass aoc-report (summary)
  ())

(defun print-table (lst &optional (stream *standard-output*))
  "Prints a list of lists as a table"
  (format stream "~&~9a ~{~3a~}"
          ""
          (alexandria:iota 25 :start 1))
  (mapcar (lambda (l)
            (format stream "~&~9a ~{~3a~}"
                    (first l)
                    (rest l)))
          lst))

(defun all-results (test)
  (loop for r across (results test)
        collect r))

(defun filter-results (results typ)
  (remove-if-not (lambda (a) (typep a typ)) results))

(defun has-parent (test)
  (null (parent (expression test))))

(defun aoc-daily-tests-recap (tests)
  (loop for test across tests
        collect (coerce (loop for inner-test across test
                              collect (cond
                                        ((and (eql :passed (status inner-test))
                                              (eql 'controlling-result (type-of inner-test))
                                              (eql :skipped (child-status inner-test))) #\s)
                                        ((eql :passed (status inner-test)) #\*)
                                        ((eql :failed (status inner-test)) #\f)))
                        'string)))

(defun test-name->day-number (test-name)
  "Takes a name like AOC2018/DAY3 (passed as a symbol) and returns the
corresponding day number (3)"
  (register-groups-bind ((#'parse-integer day))
      ("\(\\d+\)\$" (symbol-name test-name))
    day))

(defun solvedp (problems-test)
  (eql :passed (status problems-test)))


(defun summary-stats (tests)
  (let* ((solved-problems-count (loop with solved-counter = 0
                                      for year-tests being the hash-values of tests
                                      do (loop for daily-tests across year-tests
                                               do (loop for problems-test
                                                          across daily-tests
                                                        when (solvedp problems-test)
                                                          do (incf solved-counter)))
                                      finally (return solved-counter)))
         (total-problems-count (* 50 (length (alexandria:hash-table-keys tests))))
         (solved-percentage (* 100.0 (/ solved-problems-count total-problems-count)))
         (format-string "Progress: ~a/~a (~$%)"))
    (format nil format-string
            solved-problems-count total-problems-count
            solved-percentage)))

(defun print-aoc-legend-and-summary-stats (tests &optional (stream *standard-output*))
  (let* ((format-string "~&
*: Problem solved
s: Test skipped
f: Failed test

~a

"))
    (format stream format-string (summary-stats tests))))

(defmethod test-hierarchy ((report report))
  (let ((test-hierarchy (make-hash-table)))
    ;; First we set up the keys...
    (dolist (parent-test (remove-if-not #'has-parent
                                        (filter-results (all-results report)
                                                        'test-result)))
      (setf (gethash (name (expression parent-test)) test-hierarchy)
            (make-array 25 :initial-element #())))
    ;; ... then their values
    (dolist (test (remove-if #'has-parent
                             (filter-results (all-results report)
                                             'test-result)))
      (setf (aref (gethash (name (parent (expression test))) test-hierarchy)
                  (- (test-name->day-number (name (expression test))) 1))
            (results test)))
    test-hierarchy))

(defmethod summarize ((report aoc-report))
  ;; For AOC tests, we have a three-level hierarchy:
  ;; - The top level test marks the year, e.g. aoc2019
  ;;   This level has no parent
  ;;   - under that, we have individual groups for each day, e.g. aoc2019/day1
  ;;     their parent is the top level test
  ;;     - under that, there are individual test for each part of the day,
  ;;       including .../solution1 and .../solution2 that mark the two daily problems
  (let ((test-hierarchy (test-hierarchy report)))
    (print-aoc-legend-and-summary-stats test-hierarchy (output report))
    (print-table (loop for k being the hash-keys of test-hierarchy
                       for v being the hash-values of test-hierarchy
                       collect (append (list k)
                                       (aoc-daily-tests-recap v)))
                 (output report)))
  report)

(defun run-aoc-tests ()
  (parachute:test :advent-of-code/test :report 'aoc-report))

(defclass aoc-report-time (aoc-report)
  ())

(defmethod printable-aoc-test-duration (test)
  (cond
    ((and (eql :passed (status test))
          (eql 'controlling-result (type-of test))
          (eql :skipped (child-status test))) "-")
    ((eql :passed (status test)) (format nil "~,4fs" (duration test)))
    ((eql :failed (status test)) "/")))

(defmethod aoc-test-duration-hist (test)
  (cond
    ((and (eql :passed (status test))
          (eql 'controlling-result (type-of test))
          (eql :skipped (child-status test))) "")
    ((eql :passed (status test))
     (format nil "~{~a~}" (make-list (floor (/ (duration test) 0.25))
                                     :initial-element #\#)))
    ((eql :failed (status test)) "")))

(defmethod summarize ((report aoc-report-time))
  (let ((test-hierarchy (test-hierarchy report))
        (stream (output report)))
    (format stream "~&~a~%~%" (summary-stats test-hierarchy))
    (loop for year being the hash-keys of test-hierarchy
          for year-tests being the hash-values of test-hierarchy
          do (loop for idx from 0 below (length year-tests)
                   for daily-tests = (aref year-tests idx)
                   do (format stream "~&~3d - " (+ 1 idx))
                      (loop for part from 1
                            for problems-test across daily-tests
                            do (format stream "Part ~d: ~7a ~10a  " part
                                       (if (solvedp problems-test)
                                           (printable-aoc-test-duration problems-test)
                                           0)
                                       (if (solvedp problems-test)
                                           (aoc-test-duration-hist problems-test)
                                           0)))))))
