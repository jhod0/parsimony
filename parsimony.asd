(defsystem parsimony
  :description "A simple parser combinator and generator library"
  :author "Jackson O'Donnell <jacksonhodonnell@gmail.com>"
  :homepage "https://github.com/jhod0/parsimony"
  :source-control (:git "git@github.com:jhod0/parsimony.git")
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "core")
                             (:file "combinators")
                             (:file "util-parsers")
                             (:file "file-io")
                             (:module "compiler"
                              :serial t
                              :components ((:file "lexer")
                                           (:file "grammars"))))))
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