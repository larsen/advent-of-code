(in-package #:advent-of-code)

(defstruct fighter
  (hit-points 0)
  (damage 0)
  (armor 0)
  (expense 0))

(defparameter +inventory+
  '((:weapons . '((:dagger        8     4       0)
                  (:shortsword   10     5       0)
                  (:warhammer    25     6       0)
                  (:longsword    40     7       0)
                  (:greataxe     74     8       0)))
    (:armor . '((:leather      13     0       1)
                (:chainmail    31     0       2)
                (:splintmail   53     0       3)
                (:bandedmail   75     0       4)
                (:platemail   102     0       5)))
    (:rings . '((:damage-+1    25     1       0)
                (:damage-+2    50     2       0)
                (:damage-+3   100     3       0)
                (:defense-+1   20     0       1)
                (:defense-+2   40     0       2)
                (:defense-+3   80     0       3)))))

(defparameter +inventory-plist+
  '(:weapons '((:dagger        8     4       0)
               (:shortsword   10     5       0)
               (:warhammer    25     6       0)
               (:longsword    40     7       0)
               (:greataxe     74     8       0))
    :armor '((:leather      13     0       1)
             (:chainmail    31     0       2)
             (:splintmail   53     0       3)
             (:bandedmail   75     0       4)
             (:platemail   102     0       5))
    :rings '((:damage-+1    25     1       0)
             (:damage-+2    50     2       0)
             (:damage-+3   100     3       0)
             (:defense-+1   20     0       1)
             (:defense-+2   40     0       2)
             (:defense-+3   80     0       3))))



(defun equip (fighter item)
  (let ((name (first item))
        (cost (second item))
        (damage (third item))
        (armor (fourth item)))
    (format t "Equipped ~a" name)))

(defun read-fighter-specs ()
  "Reads a fighter specification from a file a returns a FIGHTER struct.
It assumes the files is well-formatted and it respects the docs (no error mgmt)"
  (labels ((read-specs-line (line)
             (parse-integer (second (split-sequence #\: line)))))
    (let ((specs (uiop:read-file-lines
                  (asdf:system-relative-pathname 'advent-of-code
                                                 "inputs/2015/day21"))))
      (make-fighter :hit-points (read-specs-line (first specs))
                    :damage (read-specs-line (second specs))
                    :armor (read-specs-line (third specs))))))
