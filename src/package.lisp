
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
           :make-parser :defparser :next :peek :fail :recurse
           :with-parsed
           :eval-in-context

           :alternative :parse-all :parse-some :parse-many
           :maybe :fulfills :while-fulfills :parse-until
           :expect-string

           :parse-digit :parse-int :parse-float :parse-char :one-of :whitespace

           :deflexer :lexer :get-lexer-parser :lexer-stream

           :defgrammar :get-grammar-parser :parse-grammar))