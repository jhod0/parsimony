

(defsystem parsimmons
  :description "A simple parser combinator and generator library"
  :author "Jackson O'Donnell <jacksonhodonnell@gmail.com>"
  :components
  ((:file "package")
   (:file "parsimmons")))

(defsystem parsimmons/tests
  :description "Test suite"
  :depends-on (:parsimmons :FiveAM)
  :components ((:module "tests"
                :serial t
                :components ((:file "package")
                             (:file "main")))))
