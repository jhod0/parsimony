;; ======= Parser Combinators =======

(in-package :parsimony)


(defparser alternative (parser &rest parsers) ()
  (labels ((try-all (p rest)
             (handler-case
              (eval-in-context p)
              (parse-failure (p)
                (if rest (try-all (car rest) (cdr rest))
                  (error p))))))
    (try-all parser parsers)))

(defparser parse-all (&rest parsers) ()
  (mapcar #'(lambda (parser) (eval-in-context parser))
          parsers))

(defparser parse-some (parser) ((fst parser))
   (cons fst (recurse :raise nil :default nil)))

(defparser parse-many (parser) ()
  (eval-in-context (parse-some parser) :raise nil :default nil))

(defparser maybe (parser) ()
  (handler-case
      (values t (eval-in-context parser))
    (parse-failure () (values nil nil))))

(defparser fulfills (predicate &optional parser) ()
  (let ((obj (if parser (eval-in-context parser) (next))))
    (if (funcall predicate obj)
        obj
      (fail obj))))

(defparser while-fulfills (predicate &optional parser) ()
  (let ((obj (if parser (eval-in-context parser) (next))))
    (if (funcall predicate obj)
        (cons obj (recurse))
      nil)))

(defparser parse-until (parser) ((e parser :raise nil))
  (if (eq e :noparse)
      (let ((obj (next)))
        (multiple-value-bind (a b) (recurse)
          (values (cons obj a) b)))
      (values nil e)))

(defparser expect-string (str) ()
  (dotimes (i (length str))
    (let ((a (next)))
      (unless (eq (aref str i) a)
        (fail a))))
  str)

(defparser pmap (fun &rest parsers) ((res (apply #'parse-all parsers)))
   (apply fun res))