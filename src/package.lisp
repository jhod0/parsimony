(defpackage :parsimony/core
  (:use :common-lisp)
  (:export :get-stream :put-stream :peek-stream :stream-location

           :*default-parse-input*

           :parse-failure
           :parse-failure-backtrace
           :print-backtrace
           :print-backtrace-reraise
           :with-parse-input

           ;; Core parsing utils
           :parser
           :parse-context :push-obj-to-context :pop-object-from-context
           :eval-parser
           :parse-loop
           :make-parser :defparser :ctxt :next :peek :fail :recurse
           :with-parsed
           :eval-in-context

           ;; Combinators
           :pmap
           :alternative :parse-all :parse-some :parse-many
           :maybe :fulfills :while-fulfills :parse-until
           :expect-string

           ;; Provided parsers
           :parse-digit :parse-int :parse-float :parse-char :parse-eof :one-of :whitespace

           :copy-file-loc :new-file-stream :fl-col :fl-row))

(defpackage :parsimony/lexer-compiler
  (:use :common-lisp :parsimony/core)
  (:export :deflexer :lexer :get-lexer-parser :lexer-stream
           :lexer-terminals))

(defpackage :parsimony/grammar-compiler
  (:use :common-lisp :parsimony/core :parsimony/lexer-compiler)
  (:export :defgrammar :get-grammar :parse-grammar))

(defpackage :parsimony
  (:use :common-lisp
        :parsimony/core
        :parsimony/lexer-compiler
        :parsimony/grammar-compiler)
  (:nicknames :prs)
  (:export :get-stream :put-stream :peek-stream

           :*default-parse-input*

           :parse-failure
           :parse-failure-backtrace
           :print-backtrace
           :print-backtrace-reraise
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