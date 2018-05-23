;; ======= Parser Combinators =======
(in-package :parsimony/core)


(defparser alternative (parser &rest parsers) ()
  "Attempts each of its argument parsers in sequence, returns the result of the
first to succeed."
  (labels ((try-all (p rest)
             (handler-case
              (eval-in-context p)
              (parse-failure (p)
                (if rest (try-all (car rest) (cdr rest))
                  (error p))))))
    (try-all parser parsers)))

(defparser parse-all (&rest parsers) ()
  "Executes each parser in sequence, and returns a list of their results."
  (mapcar #'(lambda (parser) (eval-in-context parser))
          parsers))

(defparser parse-some (parser) ((fst parser))
   "Executes `parser` repeatedly and returns a list of each value it returns.
Stops the first time `parsser` fails. Will return a non-empty list, so if
`parser` fails on the first try, `parse-some` fails."
   (cons fst (recurse :raise nil :default nil)))

(defparser parse-many (parser) ()
  "Executes `parser` repeatedly and returns a list of the results. Unlike `parse-some`,
may return nil."
  (eval-in-context (parse-some parser) :raise nil :default nil))

(defparser maybe (parser) ()
  "Attempts `parser`. If `parser` fails, returns (values nil nil), if it
succeeds, returns (values <result> t)."
  (handler-case
      (values (eval-in-context parser) t)
    (parse-failure () (values nil nil))))

(defparser fulfills (predicate &optional parser) ()
  "If the next object in stream fulfills `predicate`, returns that object,
otherwise fails.

If `parser` is provided, `fulfills` will use the result of `parser` in place of
the next object in the stream."
  (let ((obj (if parser
                 (eval-in-context parser)
                 (next))))
    (if (funcall predicate obj)
        obj
      (fail obj))))

(defparser while-fulfills (predicate &optional parser) ()
  "Returns a list of every contiguous object in the stream which fulfills
`predicate`. If `parser` is provided, uses the results of `parser` in place of
each new object in the stream.

Examples:

(while-fulfills #'is-digit)
;; On input \"1789 was the start of the French Revolution\"
;; => '(#\1 #\7 #\8 #\9)

(while-fulfills #'evenp (parse-int-ignore-whitespace))
;; On input \"2 4 42 68 3 7 9\"
;; => '(2 4 42 68)
"
  (let (out
        (prsr (or parser (make-parser nil ((a :next)) a))))
    (parse-loop-in-context ((obj prsr))
      (if (funcall predicate obj)
        (push obj out)
        (fail obj)))
    (nreverse out)))

(defparser parse-until (parser) ((e parser :raise nil))
  (if (eq e :noparse)
      (let ((obj (next)))
        (multiple-value-bind (a b) (recurse)
          (values (cons obj a) b)))
      (values nil e)))

(defparser expect-string (str) ()
  "Expects the next characters in the stream to be the string `str`. Fails
otherwise. Returns `str` on success."
  (dotimes (i (length str))
    (let ((a (next)))
      (unless (eq (aref str i) a)
        (fail a))))
  str)


(defparser pmap-parse-all-helper (input) ()
  (let ((p (car input))
        (parsers (cdr input)))
    (cond
      ((eq p :ignore)
       (progn (eval-in-context (car parsers))
              (eval-in-context (pmap-parse-all-helper (cdr parsers)))))
      (parsers (cons (eval-in-context p)
                     (eval-in-context (pmap-parse-all-helper parsers))))
      (t (list (eval-in-context p))))))

(defparser pmap (fun &rest parsers)
                ((res (pmap-parse-all-helper parsers)))
  "Parses each in `parsers` as in `parse-all`, calls `fun` on their `results`."
  (apply fun res))