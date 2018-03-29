
(defpackage :parsimmons
  (:use :common-lisp)
  (:export :get-stream :put-stream :peek-stream
           :*parse-input*

           :eval-parser
           :defparser
           :parse-loop

           :alternative :parse-all :parse-some :parse-many

           :parse-digit :parse-int :parse-char :one-of))
