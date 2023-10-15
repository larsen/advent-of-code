(in-package #:advent-of-code/test)

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

(defmethod summarize ((report aoc-report))
  ;; For AOC tests, we have a three-level hierarchy:
  ;; - The top level test marks the year, e.g. aoc2019
  ;;   This level has no parent
  ;;   - under that, we have individual groups for each day, e.g. aoc2019/day1
  ;;     their parent is the top level test
  ;;     - under that, there are individual test for each part of the day,
  ;;       including .../solution1 and .../solution2 that mark the two daily problems
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
    ;; Now pretty-printing the structure
    (format (output report)
            "~&~%
*: Problem solved
s: Test skipped
f: Failed test

")
    (print-table (loop for k being the hash-keys of test-hierarchy
                       for v being the hash-values of test-hierarchy
                       collect (append (list k)
                                       (aoc-daily-tests-recap v)))
                 (output report)))
  report)

(defun run-aoc-tests ()
  (parachute:test :advent-of-code/test :report 'aoc-report))
