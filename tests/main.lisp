
(in-package :parsimony/tests)


(def-suite parsimony-tests
    :description "all parsimony tests")

(in-suite parsimony-tests)

(defparameter *cur-parser* nil)
(defparameter *check-function* #'eq)

(defun check-parse-result (input result &optional (eq *check-function*))
  (with-input-from-string (s input)
    (is (funcall eq (prs:eval-parser *cur-parser* :input s)
                 result))))

(defmacro check-parse-results ((parser &optional checker) &rest checks)
  `(let ((*cur-parser* ,parser)
         ,@(when checker `((*check-function* ,checker))))
     ,@(mapcar #'(lambda (f) `(check-parse-result ,(car f) ,(cadr f)))
               checks)))


(def-suite numeric-tests
  :description "test small numeric parsers"
  :in parsimony-tests)

(in-suite numeric-tests)

(test test-integer
  :documentation "Tests parsing integers with library function parse-int"
  (check-parse-results ((prs:parse-int) #'=)
    ("123" 123)
    ("3324123" 3324123)
    ("42738" 42738)))

(test test-float-zeros
  :documentation "Parses floats with only zeros after the decimal"
  (check-parse-results ((prs:parse-float) #'=)
    ("134214.0" 134214.0)
    ("45.0" 45.0)
    ("67.0" 67.0)))

(test test-float-decimals
   :documentation "Tests parsing real floats"
   (check-parse-results ((prs:parse-float)
                         (lambda (a b)
                           (and (typep a 'float) (typep b 'float)
                                (< (abs (- a b))
                                   0.00001))))
     ("143.342" 143.342)
     ("0.1234" 0.1234)
     ("435.987" 435.987)))

(def-suite combinator-tests
  :description "Tests parser combinators"
  :in parsimony-tests)

(in-suite combinator-tests)

(test test-int-not-float
  :documentation "Tests Alternative with ints and floats"
  (check-parse-results ((prs:alternative (prs:parse-float) (prs:parse-int))
                        #'=)
    ("4524" 4524)
    ("643" 643)
    ("198" 198)
    ("432894" 432894)))

(def-suite literal-parser-tests
  :description "Tests parses built directly with make-parser"
  :in parsimony-tests)

(in-suite literal-parser-tests)
