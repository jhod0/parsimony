(defsystem parsimony
  :description "A simple parser combinator and generator library"
  :author "Jackson O'Donnell <jacksonhodonnell@gmail.com>"
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "core")
                             (:file "combinators")
                             (:file "util-parsers"))))
  :in-order-to ((test-op (test-op :parsimony/tests))))

(defsystem parsimony/tests
  :description "Test suite"
  :depends-on (:parsimony :FiveAM)
  :components ((:module "tests"
                :serial t
                :components ((:file "package")
                             (:file "main"))))
  :perform (test-op (o p)
             (uiop:symbol-call :fiveam :run-all-tests)))
