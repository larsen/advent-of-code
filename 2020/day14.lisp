(in-package #:advent-of-code)

(defun read-program-day14 ()
  (uiop:read-file-lines
   (asdf:system-relative-pathname 'advent-of-code "inputs/2020/day14")))

(defun apply-mask (mask value)
  (let ((value-as-binary-string (format nil "~36,'0b" value)))
    (parse-integer (coerce (loop for v across value-as-binary-string
                                for m across mask
                                collect (if (char= #\X m) v m))
                          'string)
                   :radix 2)))

(defun apply-address-mask (mask address)
  (let ((value-as-binary-string (format nil "~36,'0b" address)))
    (coerce (loop for v across value-as-binary-string
                  for m across mask
                  collect (case m
                            (#\0 v)
                            (#\1 m)
                            (#\X m)))
            'string)))

(defun aoc2020/day14/solution1 ()
  (let ((memory (make-hash-table))
        (mask nil))
    (loop for instruction in (read-program-day14)
          when (scan "^mask" instruction)
            do (register-groups-bind (t-mask)
                   ("^mask = \([01X]+\)" instruction)
                 (setf mask t-mask))
          when (scan "^mem" instruction)
            do (register-groups-bind ((#'parse-integer address value))
                   ("^mem\\[\(\\d+\)\\] = \(\\d+\)" instruction)
                 (setf (gethash address memory)
                       (apply-mask mask value))))
    (apply #'+ (hash-table-values memory))))

(defun addresses (flipping-address)
  (let ((addrs '()))
    (defun address-recur (addr)
      (if (null (scan "X" addr))
          (push (parse-integer addr :radix 2) addrs)
          (progn
            (let ((t-addr (copy-seq addr)))
              (address-recur (replace (copy-seq addr) "0" :start1 (scan "X" t-addr)))
              (address-recur (replace (copy-seq addr) "1" :start1 (scan "X" t-addr)))))))
    (address-recur flipping-address)
    addrs))

(defun aoc2020/day14/solution2 ()
  (let ((memory (make-hash-table))
        (mask nil))
    (loop for instruction in (read-program-day14)
          when (scan "^mask" instruction)
            do (register-groups-bind (t-mask)
                   ("^mask = \([01X]+\)" instruction)
                 (setf mask t-mask))
          when (scan "^mem" instruction)
            do (register-groups-bind ((#'parse-integer address value))
                   ("^mem\\[\(\\d+\)\\] = \(\\d+\)" instruction)
                 (loop for addr in (addresses (apply-address-mask mask address))
                       do (setf (gethash addr memory) value))))
    (apply #'+ (hash-table-values memory))))
