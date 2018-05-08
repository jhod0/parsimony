
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
          "Expected ~s, parsed ~s" result val))))

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

(defun full-eq (a b)
  (cond
    ((symbolp a) (eq a b))
    ((stringp a) (string= a b))
    ((numberp a) (= a b))
    ((consp a) (and (full-eq (car a) (car b))
                    (full-eq (cdr a) (cdr b))))
    (t (error "don't know how to compare ~a and ~a" a b))))

(defmacro check-grammar (grammar (input expected &key target) &rest others)
  (let ((input-name (gensym))
        (res (gensym)))
      `(progn
         (with-input-from-string (,input-name ,input)
           (let ((,res (prs:parse-grammar ,grammar
                                          ,@(when target (list :target target))
                                          :input ,input-name)))
             (is (full-eq ,res ,expected)
                 "expected ~s, received ~s" ,expected ,res)))
         ,@(when others
                 (list `(check-grammar ,grammar ,@others))))))

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

(test test-pmap
  :documentation "Tests parser map function"
  (flet ((dummy (n1 c1 n2 c2 n3)
           (is (eq c1 #\space))
           (is (eq c2 #\space))
           (+ n1 n2 n3)))
    (for-all ((n1 (gen-integer :min 0))
              (n2 (gen-integer :min 0))
              (n3 (gen-integer :min 0)))
      (check-parse-results
       ((prs:pmap #'dummy
                  (prs:parse-int) (prs:parse-char #\space)
                  (prs:parse-int) (prs:parse-char #\space)
                  (prs:parse-int))
        #'=)
       ((format nil "~d ~d ~d" n1 n2 n3) (+ n1 n2 n3)))))

#|  (for-all ((n1 (gen-integer :min 0))
            (n2 (gen-integer :min 0)))
    (check-parse-results
     ((prs:pmap #'cons
                (prs:parse-int)
                :ignore (prs:parse-char #\comma)
                (prs:parse-int))
      (lambda (a b)
        (and (consp a) (consp b)
             (= (car a) (car b))
             (= (cdr a) (cdr b)))))
     ((format nil "~d,~d" n1 n2) (cons n1 n2))))
|#)

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
  (check-parse-results ((prs:expect-string "truth") #'string=)
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
  (check-lexer (prs:get-lexer-parser prs/e:dummy-lexer)
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
  (check-lexer (prs:get-lexer-parser prs/e:small-grammar-lexer)
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
  (check-lexer (prs:get-lexer-parser prs/e:small-pascal-lexer)
    ("type smallstring = packed array[1..20] of char;"
     (:type (:ident "smallstring") :equals :packed
            :array :open-brace (:integer 1) :range-dots
            (:integer 20) :close-brace :of (:ident "char") :semicolon))
    ("record a, b ,c : sometype ;
             other: packed array[1..30] of char;
end"
     (:record (:ident "a") :comma (:ident "b") :comma (:ident "c")
              :colon (:ident "sometype") :semicolon
              (:ident "other") :colon :packed :array
              :open-brace (:integer 1) :range-dots (:integer 30) :close-brace
              :of (:ident "char") :semicolon :end))))

(test test-json-lexer
  (check-lexer (prs:get-lexer-parser prs/e:json-lexer)
    ("[]" (:open-brace :close-brace))
    ("{{234 ,:" (:open-curly :open-curly (:jsonint 234) :comma :colon))))


(def-suite simple-grammar-tests
  :in grammar-tests)

(in-suite simple-grammar-tests)


(test test-simple-grammar-mechanics
  :description "Just make sure it parses ..something.."
  (dolist (a '("[]" "hello" "\"hello\""))
    (with-input-from-string (s a)
      (prs:parse-grammar prs/e:small-grammar :input s))))

(test test-simple-grammar
  :description "Test the simple grammar"
  (check-grammar prs/e:small-grammar
    ("32" 32)
    ("123.0" 123.0)
    ("hello" "hello")
    ("\"hello\"" "hello")
    ("[]" nil)
    ("[ 1,2, 3, 4 ,5]" '(1 2 3 4 5))
    ("[1|2]" '(1 . 2))
    (":hello" '(:symbol "hello"))
    ("[:hi, :there, 1, 2, [], 453, [:a|:b]|artichoke]"
     '((:symbol "hi") (:symbol "there") 1 2 () 453 ((:symbol "a") . (:symbol "b")) . "artichoke"))))

(test test-pascal-grammar
  :description "Tests the simplified pascal grammar"
  (check-grammar prs/e:small-pascal
    ("var a, b, c: atype;
          d, e, f : anothertype ;"
     '(:vars (:var :idents ("a" "b" "c") :type "atype")
       (:var :idents ("d" "e" "f") :type "anothertype")))

    ("var a, b : array[1..30] of character;
          anothervar ,blah : ^ integer;"
     '(:vars
       (:var :idents ("a" "b")
        :type (:arraydef :packed nil :range (:range 1 30) :of "character"))
       (:var :idents ("anothervar" "blah")
        :type (:ptr "integer"))))

    ("var a : 1..100 ; b: (some, enum ,terms );"
     '(:vars
       (:var :idents ("a")
        :type (:range 1 100))
       (:var :idents ("b")
        :type (:enum "some" "enum" "terms"))))

    ("type intalias = integer ; intptr = ^intalias;"
     '(:typedefs
       (:typedef "intalias" "integer")
       (:typedef "intptr" (:ptr "intalias"))))

    ("type linkedlist = record value : integer ;
                               next : ^linkedlist
                        end ;"
     '(:typedefs
       (:typedef "linkedlist"
        (:recdef
         (:var :idents ("value") :type "integer")
         (:var :idents ("next") :type (:ptr "linkedlist"))))))
    ))