
(in-package :parsimony/tests)


(def-suite all-tests
    :description "all parsimony tests")

(in-suite all-tests)

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

(test test-integer
  :documentation "Tests parsing integers with library function parse-int"
  (check-parse-results ((prs:parse-int) #'=)
    ("123" 123)
    ("3324123" 3324123)
    ("42738" 42738)))
