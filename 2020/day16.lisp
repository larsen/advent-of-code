(in-package #:advent-of-code)

(defun parse-ranges (ranges)
  (loop for rg in (split " or " ranges)
        collect (register-groups-bind ((#'parse-integer min max))
                    ("\(\\d+\)-\(\\d+\)" rg)
                  (cons min max))))

(defun parse-rules (rules)
  (loop for r in (split "\\n" rules)
        collect (destructuring-bind (name ranges)
                    (split ": " r :limit 2)
                  (cons name (parse-ranges ranges))) ))

(defun parse-my-ticket (my-ticket)
  (mapcar #'parse-integer (split "," (cadr (split "\\n" my-ticket)))))

(defun parse-nearby-tickets (nearby-tickets)
  (loop for ticket in (split "\\n" nearby-tickets)
        for idx from 0
        ;; skipping first line
        when (> idx 0)
          collect (mapcar #'parse-integer (split "," ticket))))

(defun read-reference-document ()
  (destructuring-bind (rules my-ticket nearby-tickets)
      (split "\\n\\n" (uiop:read-file-string
                       (asdf:system-relative-pathname 'advent-of-code "inputs/2020/day16")))
    (values (parse-rules rules)
            (parse-my-ticket my-ticket)
            (parse-nearby-tickets nearby-tickets))))

(defun all-ranges (rules)
  (loop for (name . ranges) in rules
        append (loop for r in ranges
                     collect r)))

(defun completely-invalid-error (ticket rules)
  (let ((ticket-invalid-error 0)
        (all-ranges (all-ranges rules)))
    (loop for v in ticket
          when (not (some (lambda (range)
                            (and (>= v (car range))
                                 (<= v (cdr range))))
                          all-ranges))
            do (incf ticket-invalid-error v)
          finally (return ticket-invalid-error))))

(defun aoc2020/day16/solution1 ()
  (multiple-value-bind (rules my-ticket nearby-tickets)
      (read-reference-document)
    (declare (ignorable my-ticket))
    (reduce #'+ (mapcar (lambda (ticket)
                          (completely-invalid-error ticket rules))
                        nearby-tickets))))

(defun ticket-respects-ranges-for-position (ticket pos ranges)
  (let ((v (nth pos ticket)))
    (or (and (>= v (car (first ranges)))
             (<= v (cdr (first ranges))))
        (and (>= v (car (second ranges)))
             (<= v (cdr (second ranges)))))))

(defun all-tickets-respect-ranges (pos ranges tickets)
  (if (member nil
              (mapcar (lambda (ticket)
                        (ticket-respects-ranges-for-position ticket pos ranges))
                      tickets))
      0
      1))

(defun names-and-possible-places (tickets rules)
  (loop for (name . ranges) in rules
        collect (cons name
                      (loop for idx from 0 below (length (first tickets))
                            collect (all-tickets-respect-ranges idx ranges tickets)))))

(defun is-ticket-valid (ticket rules)
  (let ((all-ranges (all-ranges rules)))
    (loop for v in ticket
          when (not (some (lambda (range)
                            (and (>= v (car range))
                                 (<= v (cdr range))))
                          all-ranges))
            return nil
          finally (return t))))

(defun aoc2020/day16/solution2 ()
  (multiple-value-bind (rules my-ticket nearby-tickets)
      (read-reference-document)
    (let ((valid-tickets (remove-if-not
                          (lambda (ticket)
                            (is-ticket-valid ticket rules))
                          nearby-tickets)))
      (format t "~20a ~{~a ~}~%" "INDEX"
              (loop for idx from 0 below (length my-ticket)
                    collect (mod idx 10)))
      (format t "~a~%" "-")
      (let ((name-and-possible-places (names-and-possible-places valid-tickets rules)))
        (loop for (name . possible-places) in name-and-possible-places
              do (format t "~20a ~{~a ~}~%" name possible-places))
        ;; Now the actual solution
        )
      )))

;; (defun test ()
;;   (multiple-value-bind (rules my-ticket nearby-tickets)
;;       (read-reference-document)
;;     (print my-ticket)))

;; 4810284647569
;; (apply #'* (mapcar (lambda (n)
;;                      (nth n '(61 151 59 101 173 71 103 167 127 157 137 73 181 97 179 149 131 139 67 53)))
;;                    '(2 9 3 17 13 15)))
