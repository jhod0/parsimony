

(in-package :parsimony)


(defun digitp (c)
  (member c (coerce "0123456789" 'list)))

(defparser parse-digit () ()
  "Parses a single digit character."
  (if (digitp (peek))
    (- (char-int (next)) (char-int #\0))
    (fail (peek))))

(defparser parse-int () ()
  "Parses an integer, fails if the next input is not a digit."
  (let ((p (parse-digit)))
    (do ((d (eval-in-context p) (eval-in-context p :raise nil))
         (n 0))
      ((eq d :noparse) n)
      (setf n (* n 10))
      (incf n d))))

(defparser parse-char (c) ((cin :next))
  "Fails if the next char in input is not `c`."
  (if (eq cin c)
      c
      (fail cin)))

(defparser parse-eof () ((c :next))
   "Fails if input is not at EOF."
   (if (eq c :eof)
     :eof
     (fail c)))

(defparser parse-float ()
  ((big (parse-int))
   (:ignore (parse-char #\.))
   (little (parse-int)))
  "Parses a float, in the form \"<digits>.<digits>\"."
  (labels ((decimal (n)
             (if (< n 1)
                 n
                 (decimal (/ n 10)))))
    (+ big (decimal (coerce little 'float)))))

(defparser one-of (lst) ((c :next))
  "Fails if the next token of input is not in `lst`."
  (if (member c (coerce lst 'list))
    c
    (fail c)))
