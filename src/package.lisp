
(defpackage :parsimony
  (:use :common-lisp)
  (:export :get-stream :put-stream :peek-stream

           :*default-parse-input*

           :parse-failure
           :with-parse-input

           :eval-parser
           :parse-loop
           :make-parser :defparser :next :peek :fail
           :eval-in-context

           :alternative :parse-all :parse-some :parse-many

           :parse-digit :parse-int :parse-char :one-of))
