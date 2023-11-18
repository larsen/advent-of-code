(in-package #:advent-of-code)

(defun read-polymer-template ()
  (destructuring-bind (template rules-raw)
      (split "\\n\\n"
             (uiop:read-file-string
              (asdf:system-relative-pathname
               'advent-of-code "inputs/2021/day14")))
    (values template
            (mapcar (lambda (rule-raw)
                      (destructuring-bind (from to)
                          (split " -> " rule-raw)
                        (cons from (char to 0))))
                    (split "\\n" rules-raw)))))

(defun apply-rule (template rules)
  (coerce
   (append
    (loop for pos from 0 below (- (length template) 1)
          for substring = (subseq template pos (+ pos 2))
          append (list (char substring 0) (cdr (assoc substring rules :test 'equalp))))
    (list (char (subseq template (- (length template) 1)) 0)))
   'string))


(defun two-chars-to-string (c1 c2)
  (coerce (list c1 c2) 'string))


(defun aoc2021/day14/solution1 ()
  (multiple-value-bind (template rules)
      (read-polymer-template)
    (let ((result template)
          (occurrencies (make-hash-table)))
      (loop repeat 10
            do (setf result (apply-rule result rules)))
      (loop for c across result
            do (incf (gethash c occurrencies 0))
            finally (progn (return (- (apply #'max (hash-table-values occurrencies))
                                      (apply #'min (hash-table-values occurrencies)))))))))

(defun apply-rules-repeteadly (template times rules)
  "Apply RULES, TIMES times, to the first two characters of TEMPLATE"
  (let ((first-char (aref template 0)))
    (loop for result = template
            then (two-chars-to-string first-char
                                      (cdr (assoc result rules :test 'equalp)))
          repeat times
          finally (return result))))


(defun aoc2021/day14/solution2 ()
  (let ((occurrencies (make-hash-table)))
    (multiple-value-bind (template rules)
        (read-polymer-template)

      ;; (loop with last-character-written = (aref template 0)
      ;;       for c across template
      ;;       for pos from 0 below (- (length template) 1)
      ;;       do (loop for level from (- 11 1) downto 1
      ;;                do (format t "~a~%" last-character-written)
      ;;                   (incf (gethash last-character-written occurrencies 0))
      ;;                   (setf last-character-written
      ;;                         (enumerate-heads
      ;;                          (two-chars-to-string last-character-written
      ;;                                               (enumerate-heads
      ;;                                                (two-chars-to-string (aref template pos)
      ;;                                                                     (aref template (+ 1 pos)))
      ;;                                                level
      ;;                                                rules))
      ;;                          level rules)))

      ;;       finally (return (- (apply #'max (hash-table-values occurrencies))
      ;;                          (apply #'min (hash-table-values occurrencies)))))

      (loop with next-character = (aref template 0)
            for pos from 0 below (- (length template) 1)
            do (print (subseq template pos (+ pos 2)))
            ;; do (print (subseq template pos (+ pos 2)))
            ;; do (loop for level from 10 downto 1
            ;;          do (print next-character)
            ;;             (print (aref (apply-rules-repeteadly
            ;;                           (two-chars-to-string next-character
            ;;                                                (apply-rules-repeteadly (subseq template pos (+ pos 2))
            ;;                                                                        (- level 1) rules))
            ;;                           1 rules)
            ;;                          0))
            ;;              (print (aref (apply-rules-repeteadly
            ;;                           (two-chars-to-string next-character
            ;;                                                (apply-rules-repeteadly (subseq template pos (+ pos 2))
            ;;                                                                        (- level 1) rules))
            ;;                           1 rules)
            ;;                          1))))

      ))))
