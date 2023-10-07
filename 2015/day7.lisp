(in-package #:advent-of-code)

(defun parse-circuit-line (line)
  (register-groups-bind (signal wire)
      ("(.+) -> (\\w+)" line)
    (values wire signal)))

(defun read-circuit ()
  (let ((circuit (make-hash-table :test 'equal)))
    (loop for line in (uiop:read-file-lines
                       (asdf:system-relative-pathname 'advent-of-code
                                                      "inputs/2015/day7"))
          do (multiple-value-bind (wire signal)
                 (parse-circuit-line line)
               (setf (gethash wire circuit) signal))
          finally (return circuit))))

(defun probe (wire)
  (let ((wire-signal (gethash wire *circuit*)))
    (unless wire-signal
      (error "Unknown wire ~a" wire))
    (or
     (register-groups-bind ((#'parse-integer value))
         ("^(\\d+)$" wire-signal)
       value)
     (register-groups-bind (wire2)
         ("^(\\w+)$" wire-signal)
       (probe wire2))
     (register-groups-bind (wire1)
         ("^NOT (\\w+)$" wire-signal)
       (lognot
        (probe wire1)))
     (register-groups-bind (wire1 wire2)
         ("^(\\w+) OR (\\w+)$" wire-signal)
       (logior
        (probe wire1)
        (probe wire2)))
     (register-groups-bind ((#'parse-integer value)
                            wire1)
         ("^(\\d+) AND (\\w+)$" wire-signal)
       (logand
        value
        (probe wire1)))
     (register-groups-bind (wire1 wire2)
         ("^(\\w+) AND (\\w+)$" wire-signal)
       (logand
        (probe wire1)
        (probe wire2)))
     (register-groups-bind (wire1
                            (#'parse-integer displacement))
         ("^(\\w+) LSHIFT (\\d+)$" wire-signal)
       (ash (probe wire1)
            displacement))
     (register-groups-bind (wire1
                            (#'parse-integer displacement))
         ("^(\\w+) RSHIFT (\\d+)$" wire-signal)
       (ash (probe wire1)
            (* -1 displacement))))))

(defvar *circuit*)

(defun aoc2015/day7/solution1 ()
  (when (function-memoized-p 'probe)
    (unmemoize-function 'probe))
  (memoize-function 'probe :test #'equal)
  (setf *circuit* (read-circuit))
  (probe "a"))

(defun aoc2015/day7/solution2 ()
  (when (function-memoized-p 'probe)
    (unmemoize-function 'probe))
  (memoize-function 'probe :test #'equal)
  (setf *circuit* (read-circuit))
  (setf (gethash "b" *circuit*) "16076")
  (probe "a"))
