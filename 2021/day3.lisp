(in-package #:advent-of-code)

(defun read-diagnostic-report ()
  (mapcar #'string-to-bit-vector
          (uiop:read-file-lines (asdf:system-relative-pathname
                                 'advent-of-code "inputs/2021/day3"))))

(defun string-to-bit-vector (str)
  (make-array (length str)
              :element-type 'bit
              :initial-contents (mapcar #'digit-char-p (coerce str 'list))))

(defun bit-vector-to-number (bit-vector)
  (reduce (lambda (acc bit)
            (+ (ash acc 1) bit))
          bit-vector))

(defun most-frequent-binary-digit-in-position (report position)
  (if (>= (count 1 (mapcar (lambda (bit-vector)
                             (aref bit-vector position))
                           report))
          (/ (length report) 2))
      1
      0))

(defun aoc2021/day3/solution1 ()
  (let* ((report (read-diagnostic-report))
         (diagnostic-number-len (length (first report)))
         (gamma-rate
           (make-array diagnostic-number-len
                       :element-type 'bit
                       :initial-contents (loop for position from 0 below diagnostic-number-len
                                               collect (most-frequent-binary-digit-in-position
                                                        report position))))
         (epsilon-rate (bit-not gamma-rate)))
    (* (bit-vector-to-number gamma-rate)
       (bit-vector-to-number epsilon-rate))))

(defun frequency-based-filter (report position comparison-op)
  (if (= (length report) 1)
      (first report)
      (frequency-based-filter
       (remove-if
        (lambda (bit-vector)
          (funcall comparison-op (aref bit-vector position)
                   (most-frequent-binary-digit-in-position
                    report position)))
        report)
       (+ position 1)
       comparison-op)))

(defun find-oxygen-generator-rating (report)
  (frequency-based-filter report 0 #'/=))

(defun find-co2-scrubber-rating (report)
  (frequency-based-filter report 0 #'=))

(defun aoc2021/day3/solution2 ()
  (let* ((report (read-diagnostic-report))
         (oxygen-generator-rating (find-oxygen-generator-rating report))
         (co2-scrubber-rating (find-co2-scrubber-rating report)))
    (* (bit-vector-to-number oxygen-generator-rating)
       (bit-vector-to-number co2-scrubber-rating))))
