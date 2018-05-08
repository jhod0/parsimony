
(in-package :parsimony/examples)

(prs:deflexer arith-lexer
    :whitespace (prs:alternative (prs:parse-char #\space)
                                 (prs:parse-char #\tab))
    :terminals
    ((:number (prs:parse-int))

     (:plus (prs:parse-char #\+))
     (:minus (prs:parse-char #\-))

     (:times (prs:parse-char #\*))
     (:division (prs:parse-char #\/))))

(prs:defgrammar arith
    :lexer arith-lexer
    :rules
    ((:expr
      (((:term term) (:eprime e))
       (if e
           (list term e)
           term)))

     (:eprime
      ((:plus (:term term) (:eprime e))
       (if e
           (list :plus term e)
           (list :plus term)))
      ((:minus (:term term) (:eprime e))
       (if e
           (list :minus term e)
           (list :minus term)))
      (() nil))

     (:term
      (((:form form) (:termprime blah))
       (if blah
           (list form blah)
           form)))

     (:termprime
      ((:times (:form f) (:termprime tp))
       (list :times f tp))
      ((:division (:form f) (:termprime tp))
       (list :division f tp))
      (() nil))

     (:form :number)
     ))