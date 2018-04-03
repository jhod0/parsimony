
(in-package :parsimony/tests)


(def-suite parsimony-tests
    :description "all parsimony tests")

(def-suite core-tests
  :description "Tests of core parsing functionality"
  :in parsimony-tests)

(def-suite grammar-tests
  :description "Tests of lexers and grammars"
  :in parsimony-tests)

(defparameter *cur-parser* nil)
(defparameter *check-function* #'eq)

(defun check-parse-result (input result &optional (eq *check-function*))
  (with-input-from-string (s input)
    (let ((val (prs:eval-parser *cur-parser* :input s)))
      (is (funcall eq val result)
          "Expected ~a, parsed ~a" result val))))

(defmacro check-parse-results ((parser &optional checker) &rest checks)
  `(let ((*cur-parser* ,parser)
         ,@(when checker `((*check-function* ,checker))))
     ,@(mapcar #'(lambda (f) `(check-parse-result ,(car f) ,(cadr f)))
               checks)))

(defmacro check-parse-fails (parser &rest checks)
  `(progn
     ,@(mapcar #'(lambda (f)
                   `(with-input-from-string (s ,f)
                      (signals prs:parse-failure
                             (prs:eval-parser ,parser :input s))))
               checks)))

(defmacro check-lexer (parser (input (token value)) &rest others)
  `(progn
     (with-input-from-string (s ,input)
       (prs:with-parsed (s)
                        (((token value) ,parser))
         (is (eq token ,token) "expected token ~a, got ~a"
             ,token token)
         (is (funcall (cond
                       ((typep value 'string) #'string=)
                       ((typep value 'number) #'=)
                       (t #'eq))
                      value ,value)
             "expected value ~a, got ~a" ,value value)))
     ,@(when others
         `((check-lexer ,parser ,@others)))))
              
(def-suite numeric-tests
  :description "test small numeric parsers"
  :in core-tests)

(in-suite numeric-tests)

(test test-integer
  :documentation "Tests parsing integers with library function parse-int"
  (for-all ((n (gen-integer :min 0 :max 1000000)))
    (let ((*cur-parser* (prs:parse-int)))
      (check-parse-result (format nil "~d" n) n))))

(test test-float-zeros
  :documentation "Parses floats with only zeros after the decimal"
  (for-all ((n (gen-integer :min 0 :max 1000000)))
    (let ((*cur-parser* (prs:parse-float)))
      (check-parse-result (format nil "~d.0" n) n #'=))))

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

(test test-float-failure
   :documentation "Tests that floats handle invalid input correctly"
   (check-parse-fails (prs:parse-float)
     "1234a" "323x0" "axbcdser"))


(def-suite combinator-tests
  :description "Tests parser combinators"
  :in core-tests)

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
  :in core-tests)

(in-suite literal-parser-tests)


(def-suite simple-lexer-tests
  :in grammar-tests)

(in-suite simple-lexer-tests)

(prs:deflexer dummy-lexer
  :documentation "Test a dummy lexer"
  :whitespace (prs:parse-char #\space)
  :terminals
  ((:float (prs:parse-float))
   (:int (prs:parse-int))
   (:newline (prs:parse-char #\newline))
   (:ident ((name
             (prs:parse-some (prs:one-of "abcdefghijklmnopqrstuvwxyz"))))
           (coerce name 'string))))

(test test-dummy-lexer
  :description "Test a simple lexer"
  (check-lexer (prs:get-lexer-parser dummy-lexer)
    ("23413" (:int 23413))
    ("23.123" (:float 23.123))
    ("
" (:newline #\newline))
    ("abcd" (:ident "abcd"))
    ("  456 " (:int 456))
    (" 23.123  " (:float 23.123))))
