(in-package #:advent-of-code)

(defstruct monkey
  (identifier)
  (items '())
  (operation-f)
  (operation-operand)
  (divisibility-test)
  (test-true-dest)
  (test-false-dest)
  (items-inspected 0))

(defmethod print-object ((m monkey) stream)
  (with-slots (identifier items operation-f operation-operand items-inspected) m
    (format stream "ID:~a ~{~a~^, ~} f:~a~a~% (inspected ~a)"
            identifier
            items
            operation-f
            operation-operand
            items-inspected)))

(defun monkey-from-string (str)
  (register-groups-bind ((#'parse-integer id)
                         items
                         operation-f
                         operation-operand
                         (#'parse-integer divisibility-test
                                          test-true-dest
                                          test-false-dest))
      ("Monkey (\\d+):
  Starting items: (.+)
  Operation: new = old (.) (.+)
  Test: divisible by (\\d+)
    If true: throw to monkey (\\d+)
    If false: throw to monkey (\\d+)" str)
    (make-monkey :identifier id
                 :items (mapcar #'parse-integer (split ", ?" items))
                 :operation-f operation-f
                 :operation-operand operation-operand
                 :divisibility-test divisibility-test
                 :test-true-dest test-true-dest
                 :test-false-dest test-false-dest)))

(defmethod throw-item (item (to monkey))
  (setf (monkey-items to) (nconc (monkey-items to) (list item))))

(defmethod apply-operation (item (m monkey))
  (with-slots (operation-f operation-operand) m
    (funcall (intern operation-f)
             item
             (if (string= "old" operation-operand)
                 item
                 (parse-integer operation-operand)))))

(defmethod test-divisibility (item (m monkey))
  (zerop (mod item (monkey-divisibility-test m))))

(defun read-monkeys-team ()
  (mapcar #'monkey-from-string
          (split "\\n\\n"
                 (uiop:read-file-string
                  (asdf:system-relative-pathname :advent-of-code "inputs/2022/day11")))))

(defun aoc2022/day11/solution1 ()
  (let ((team (read-monkeys-team)))
    (loop repeat 20
          do (loop for m in team
                   do (loop while (monkey-items m)
                            for item = (pop (monkey-items m))
                            do (incf (monkey-items-inspected m))
                               (setf item (floor (/ (apply-operation item m) 3)))
                               (if (test-divisibility item m)
                                   (throw-item item (nth (monkey-test-true-dest m)
                                                         team))
                                   (throw-item item (nth (monkey-test-false-dest m)
                                                         team)))))
          finally (return (apply #'* (subseq
                                      (sort
                                       (mapcar #'monkey-items-inspected team) #'>)
                                      0 2))))))

(defun n->list-of-remainder (n team)
  (loop for m in team
        collect (rem n (monkey-divisibility-test m))))

(defun r+ (n1r n2r team)
  (loop for n1 in n1r
        for n2 in n2r
        for m in team
        collect (rem (+ n1 n2) (monkey-divisibility-test m))))

(defun r* (n1r n2r team)
  (loop for n1 in n1r
        for n2 in n2r
        for m in team
        collect (rem (* n1 n2) (monkey-divisibility-test m))))

(defmethod apply-operation-on-remainder (item (m monkey) team)
  (with-slots (operation-f operation-operand) m
    (funcall (if (string= "+" operation-f)
                 #'r+
                 #'r*)
             item
             (if (string= "old" operation-operand)
                 item
                 (n->list-of-remainder (parse-integer operation-operand)
                                       team))
             team)))

(defun aoc2022/day11/solution2 ()
    (let ((team (read-monkeys-team)))
      (loop for m in team
            do (setf (monkey-items m)
                     (mapcar (lambda (n)
                               (n->list-of-remainder n team))
                             (monkey-items m))))
      (loop repeat 10000
            do (loop for m_counter from 0
                     for m in team
                     do (loop while (monkey-items m)
                              for item = (pop (monkey-items m))
                              do (incf (monkey-items-inspected m))
                                 (setf item (apply-operation-on-remainder item m team))
                                 (if (zerop (nth m_counter item))
                                     (throw-item item (nth (monkey-test-true-dest m)
                                                           team))
                                     (throw-item item (nth (monkey-test-false-dest m)
                                                           team)))))
            finally (return (apply #'* (subseq
                                        (sort
                                         (mapcar #'monkey-items-inspected team) #'>)
                                        0 2))))))
