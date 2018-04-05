
(in-package :parsimony/tests)

(def-suite parsimony-tests
  :description "all parsimony tests")

(defun run-parsimony-tests! ()
  (run! 'parsimony-tests))

(def-suite core-tests
  :description "Tests of core parsing functionality"
  :in parsimony-tests)

(def-suite grammar-tests
  :description "Tests of lexers and grammars"
  :in parsimony-tests)

(defparameter *cur-parser* nil)
(defparameter *check-function* #'eq)

(defun check-result (input result &optional (eq *check-function*))
  (with-input-from-string (s input)
    (let ((val (prs:eval-parser *cur-parser* :input s)))
      (is (funcall eq val result)
          "Expected ~a, parsed ~a" result val))))

(defun check-fails (input)
  (with-input-from-string (s input)
    (signals prs:parse-failure
             (prs:eval-parser *cur-parser* :input s))))


(defmacro check-parse-results ((parser &optional checker) &rest checks)
  `(let ((*cur-parser* ,parser)
         ,@(when checker `((*check-function* ,checker))))
     ,@(mapcar #'(lambda (f) `(check-result ,(car f) ,(cadr f)))
               checks)))

(defmacro check-parse-fails (parser &rest checks)
  `(let ((*cur-parser* ,parser))
     ,@(mapcar #'(lambda (f)
                   `(check-fails ,f))
               checks)))

(defmacro check-lexer (parser (input expected) &rest others)
  `(progn
     (with-input-from-string (s ,input)
       (let ((expected-toks (list ,@(loop for tok in expected
                                          collect (if (keywordp tok)
                                                      tok
                                                    (car tok)))))
             (expected-tokvals (list ,@(loop for tok in expected
                                             collect (if (keywordp tok)
                                                         nil
                                                       `(list ,(cadr tok)))))))
         (prs:parse-loop (s)
                     (((token value) ,parser))
           (let ((exp-tok (pop expected-toks))
                 (exp-tokval (pop expected-tokvals)))
             (is (eq token exp-tok) "expected token ~a, got ~a"
                 exp-tok token)
             (when exp-tokval
                 (is (funcall (cond
                               ((typep value 'string) #'string=)
                               ((typep value 'number) #'=)
                               (t #'eq))
                              value (car exp-tokval))
                     "expected value ~a, got ~a" exp-tokval value))))
         (is (not expected-toks))
         (is (not expected-tokvals))))
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
      (check-result (format nil "~d" n) n))))

(test test-float-zeros
  :documentation "Parses floats with only zeros after the decimal"
  (for-all ((n (gen-integer :min 0 :max 1000000)))
    (let ((*cur-parser* (prs:parse-float)))
      (check-result (format nil "~d.0" n) n #'=))))

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

(test test-fulfills-even
  :documentation "Tests the fulfills combinator on even numbers"
  (let ((*cur-parser* (prs:fulfills #'evenp (prs:parse-int)))
        (*check-function* #'=))
    (for-all ((n (gen-integer :min 0)))
      (if (evenp n)
          (check-result (format nil "~d" n) n)
        (check-fails (format nil "~d" n))))))

(test test-expect-literal
  :description "Tests searching for a string literal"
  (check-parse-fails (prs:expect-string "truth")
    "lies" "falsehoods" "fake news"
    "tru" "trutb" "tea time")
  (check-parse-results ((prs:expect-string "truth"))
    ("truth" "truth")
    ("truth   " "truth")
    ("truthaskfjlasdf" "truth")))

(def-suite literal-parser-tests
  :description "Tests parses built directly with make-parser"
  :in core-tests)

(in-suite literal-parser-tests)

(def-suite simple-lexer-tests
  :in grammar-tests)

(in-suite simple-lexer-tests)

(test test-dummy-lexer
  :description "Test a simple lexer"
  (check-lexer (prs:get-lexer-parser dummy-lexer)
    ("23413" ((:int 23413)))
    ("23.123" ((:float 23.123)))
    ("
" ((:newline #\newline)))
    ("abcd" ((:ident "abcd")))
    ("  456 " ((:int 456)))
    (" 23.123  " ((:float 23.123)))
    ("abc 234.123 324"
     ((:ident "abc")
      (:float 234.123)
      (:int 324)))
    ("12 12
hello there"
     ((:int 12) (:int 12)
      (:newline #\newline)
      (:ident "hello")
      (:ident "there")))))

(test test-small-lexer
  :description "Test lexer for small grammar to come"
  (check-lexer (prs:get-lexer-parser small-grammar-lexer)
    ("[1,2,3,4]"
     (:open-brace (:integer 1) :comma (:integer 2) :comma (:integer 3) :comma (:integer 4) :close-brace))
    ("[1|2]"
     (:open-brace (:integer 1) :pipe (:integer 2) :close-brace))
    ("[ :hello , 234, there, 5|:a]"
     (:open-brace :colon
      (:ident "hello") :comma
      (:integer 234) :comma
      (:ident "there") :comma
      (:integer 5) :pipe :colon
      (:ident "a") :close-brace))
    (" \"hey\" : = :     []()"
     ((:string "hey")
      :colon :equals :colon
      :open-brace :close-brace :open-paren :close-paren))))

(test test-pascal-lexer
  (check-lexer (prs:get-lexer-parser small-pascal-lexer)
    ("type smallstring = packed array[1..20] of char;"
     (:type (:ident "smallstring") :equals :packed
            :array :open-brace (:integer 1) :range-dots
            (:integer 20) :close-brace :of (:ident "char") :semicolon))))


(def-suite simple-grammar-tests
  :in grammar-tests)

(in-suite simple-grammar-tests)

(test test-simple-grammar-mechanics
  :description "Just make sure it parses ..something.."
  (dolist (a '("[]" "hello" "\"hello\""))
    (with-input-from-string (s a)
      (prs:parse-grammar small-grammar :input s))))