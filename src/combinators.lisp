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
   (cons fst (eval-in-context (parse-many parser))))

(defparser parse-many (parser)
  ((v (parse-some parser) :raise nil :default nil))
  v)

(defparser maybe (parser) ()
  (handler-case
      (values t (eval-in-context parser))
    (parse-failure () (values nil nil))))
