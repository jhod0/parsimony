;; ======= Parser Combinators =======

(in-package :parsimony)

(defparser alternative (&rest parsers) ()
  (block alt
    (dolist (p parsers)
      (let ((v (eval-in-context p :raise nil)))
        (unless (eq v :noparse)
          (return-from alt v))))
    (fail)))

(defparser parse-all (&rest parsers) ()
  (mapcar #'(lambda (parser) (eval-in-context parser))
          parsers))

(defparser parse-some (parser) ((fst parser))
   (cons fst (eval-in-context (parse-many parser))))

(defparser parse-many (parser)
  ((v (parse-some parser) :raise nil :default nil))
  v)

(defparser maybe (parser) ()
  (handler-case
      (values t (eval-in-context parser))
    (parse-failure () (values nil nil))))
