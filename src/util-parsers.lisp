

(in-package :parsimony)


(defun digitp (c)
  (member c (coerce "0123456789" 'list)))

(defparser parse-digit () ()
  (if (digitp (peek))
    (- (char-int (next)) (char-int #\0))
    (fail (peek))))

(defparser parse-int () ()
  (let ((p (parse-digit)))
    (do ((d (eval-in-context p) (eval-in-context p :raise nil))
         (n 0))
      ((eq d :noparse) n)
      (setf n (* n 10))
      (incf n d))))

(defparser parse-char (c) ()
  (if (eq (peek) c)
      (next) (fail (peek))))

(defparser parse-eof () ()
   (let ((c (next)))
     (unless (eq c :eof)
       (fail c))
     :eof))

(defparser parse-float ()
  ((big (parse-int))
   (:ignore (parse-char #\.))
   (little (parse-int)))
  (labels ((decimal (n)
             (if (< n 1)
                 n
                 (decimal (/ n 10)))))
    (+ big (decimal (coerce little 'float)))))

(defparser one-of (lst) ()
  (let ((c (next)))
    (if (member c (coerce lst 'list))
      c
      (fail c))))