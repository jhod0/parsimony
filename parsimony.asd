(defsystem parsimony
  :description "A simple parser combinator and generator library"
  :author "Jackson O'Donnell <jacksonhodonnell@gmail.com>"
  :components ((:module "src"
                :components ((:file "package")
                             (:file "core" :depends-on ("package"))
                             (:file "combinators" :depends-on ("core"))
                             (:file "util-parsers" :depends-on ("core" "combinators"))
                             (:file "lexer" :depends-on ("core" "combinators" "util-parsers"))
                             (:file "grammars"
                                    :depends-on ("core" "combinators" "util-parsers" "lexer")))))
  :in-order-to ((test-op (test-op :parsimony/tests))))

(defsystem parsimony/examples
  :description "Parsimony examples"
  :depends-on (:parsimony)
  :components ((:module "examples"
                :serial t
                :components ((:file "package")
                             (:file "grammars")))))

(defsystem parsimony/tests
  :description "Test suite"
  :depends-on (:parsimony/examples :FiveAM)
  :components ((:module "tests"
                :serial t
                :components ((:file "package")
                             (:file "main"))))
  :perform (test-op (o p)
             (uiop:symbol-call :parsimony/tests :run-parsimony-tests!)))