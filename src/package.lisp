
(defpackage :parsimony
  (:use :common-lisp)
  (:nicknames :prs)
  (:export :get-stream :put-stream :peek-stream

           :*default-parse-input*

           :parse-failure
           :parse-failure-backtrace
           :with-parse-input

           :eval-parser
           :parse-loop
           :make-parser :defparser :next :peek :fail
           :with-parsed
           :eval-in-context

           :alternative :parse-all :parse-some :parse-many :maybe

           :parse-digit :parse-int :parse-float :parse-char :one-of :whitespace))
