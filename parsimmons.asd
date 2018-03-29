

(defsystem parsimmons
  :description "A simple parser combinator and generator library"
  :author "Jackson O'Donnell <jacksonhodonnell@gmail.com>"
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "core")))))

(defsystem parsimmons/tests
  :description "Test suite"
  :depends-on (:parsimmons :FiveAM)
  :components ((:module "tests"
                :serial t
                :components ((:file "package")
                             (:file "main")))))
