(in-package #:advent-of-code)

(defun read-room-list ()
  (read-input-file-as-lines "inputs/2016/day4"))

;; "vxupkizork-sgmtkzoi-pkrrehkgt-zxgototm-644[kotgr]"
(defun room-id (room)
  (register-groups-bind ((#'parse-integer room-id))
      ("-(\\d+)\\[" room)
    room-id))

(defun by-frequency-and-alphabetically (e1 e2)
  (if (= (cdr e1) (cdr e2))
      (char<= (car e1) (car e2))
      (>= (cdr e1) (cdr e2))))

(defun most-frequent-letters (letters-and-id)
  (let ((freq-table (make-hash-table :test 'equal)))
    (loop for c across letters-and-id
          when (alpha-char-p c)
            do (incf (gethash c freq-table 0)))
    (coerce (mapcar #'car (subseq (sort (loop for k being the hash-keys of freq-table
                                                using (hash-value v)
                                              collect (cons k v))
                                        #'by-frequency-and-alphabetically)
                                  0 5))
            'string)))

(defun real-room? (room)
  (register-groups-bind (first-part checksum)
      ("(.+)\\[(\\w+)\\]" room)
    (string= checksum (most-frequent-letters first-part))))

(defun real-rooms (room-list)
  (delete-if-not #'real-room? room-list))

(defun aoc2016/day4/solution1 ()
  (apply #'+ (mapcar #'room-id (real-rooms (read-room-list)))))

(defun shift (character n)
  (let ((base-code (char-code #\a)))
    (if (char= character #\-)
        #\Space
        (code-char
         (+ base-code (mod (+ n (- (char-code character) base-code))
                           26))))))

(defun decrypt-room-name (room)
  (let ((room-id (room-id room)))
    (coerce (loop for c across room
                  collect (shift c room-id))
            'string)))

(defun aoc2016/day4/solution2 ()
  (loop for room in (read-room-list)
        when (string= (decrypt-room-name room)
                      "northpole object storage pklboterhd")
          return (room-id room)))
