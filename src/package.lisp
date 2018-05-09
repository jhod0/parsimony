
(defpackage :parsimony
  (:use :common-lisp)
  (:nicknames :prs)
  (:export :get-stream :put-stream :peek-stream

           :*default-parse-input*

           :parse-failure
           :parse-failure-backtrace
           :with-parse-input

           ;; Core parsing utils
           :eval-parser
           :parse-loop
           :make-parser :defparser :next :peek :fail :recurse
           :with-parsed
           :eval-in-context

           ;; Combinators
           :pmap
           :alternative :parse-all :parse-some :parse-many
           :maybe :fulfills :while-fulfills :parse-until
           :expect-string

           ;; Provided parsers
           :parse-digit :parse-int :parse-float :parse-char :parse-eof :one-of :whitespace

           ;; Lexers
           :deflexer :lexer :get-lexer-parser :lexer-stream

           ;; Grammars
           :defgrammar :get-grammar-parser :parse-grammar))