
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
  (handler-bind
      ((prs:parse-failure
        #'(lambda (c)
            (declare (ignore c))
            (let ((restart (find-restart 'prs:print-backtrace-reraise)))
              (format *error-output* "======~%Parse-failure on input ~s~%======~%" input)
              (assert restart)
              (invoke-restart restart)))))
    (let ((val (prs:eval-parser *cur-parser* :input input)))
      (is (funcall eq val result)
          "Expected ~s, parsed ~s" result val))))

(defun check-fails (input)
  (signals prs:parse-failure
           (prs:eval-parser *cur-parser* :input input)))

(defun full-eq (a b)
  (cond
    ((symbolp a) (eq a b))
    ((stringp a) (string= a b))
    ((numberp a) (= a b))
    ((characterp a) (eq a b))
    ((consp a) (and (full-eq (car a) (car b))
                    (full-eq (cdr a) (cdr b))))
    (t (error "don't know how to compare ~s and ~s" a b))))

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
             (is (eq token exp-tok) "expected token ~s, got ~s"
                 exp-tok token)
             (when exp-tokval
                 (is (full-eq value (car exp-tokval))
                     "expected value ~s, got ~s" (car exp-tokval) value))))
         (is (not expected-toks))
         (is (not expected-tokvals))))
     ,@(when others
             `((check-lexer ,parser ,@others)))))

(defmacro check-lexer-locations (lexer (input &rest tokens) &rest inputs)
  `(progn
     (with-input-from-string (s ,input)
       (let ((lex-stream (prs:lexer-stream ,lexer :input s))
             (expected ',tokens))
         (loop while expected do
              (multiple-value-bind (type val loc)

                  (handler-bind
                      ((prs:parse-failure
                        #'(lambda (c)
                            (declare (ignore c))
                            (invoke-restart 'prs:print-backtrace-reraise))))
                    (prs:get-stream lex-stream nil))

                (declare (ignore val))
                (let* ((this-exp (pop expected))
                       (exp-type (car this-exp))
                       (row (cadr this-exp))
                       (col (caddr this-exp)))
                  (is (eq type exp-type))
                  (is (= (prs::fl-row loc) row))
                  (is (= (prs::fl-col loc) col)))))))
     ,@(when inputs (list `(check-lexer-locations ,lexer ,@inputs)))))

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

  (for-all ((n1 (gen-integer :min 0))
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
     ((format nil "~d,~d" n1 n2) (cons n1 n2)))))

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
          (check-fails (format nil "~d" n))))
    (for-all ((str (gen-string
                    :elements
                    (gen-character
                     :code (gen-integer
                            :min (1+ (char-code #\9))
                            :max (1- char-code-limit))))))
             (check-fails (format nil "~d" str)))))

(test test-while-fulfills
  :documentation "Tests the while-fulfills combinator."

  ;; Generates lists of integers, like "1234 32 231 1234",
  ;; and checks for evens
  (labels ((list-eq (a b)
             (if (or (null a) (null b))
                 (and (null a) (null b))
                 (and (= (car a) (car b))
                      (list-eq (cdr a) (cdr b)))))
           (list-to-string (lst)
             (if (= 1 (length lst))
                 (format nil "~d" (car lst))
                 (concatenate 'string
                              (format nil "~d " (car lst))
                              (list-to-string (cdr lst)))))
           (take-while (pred lst)
             (cond
               ((null lst) nil)
               ((funcall pred (car lst))
                (cons (car lst) (take-while pred (cdr lst))))
               (t nil))))

    (let ((*cur-parser* (prs:while-fulfills
                         #'evenp
                         (prs:make-parser
                          nil ((int (prs:parse-int))
                               (:ignore (prs:maybe (prs:parse-char #\space))))
                          int)))
          (*check-function* #'list-eq))
      (for-all ((lst
                 (gen-list :length (gen-integer :min 0 :max 1000)
                           :elements (gen-integer :min 0 :max 1000000))))
        (let ((str (list-to-string lst)))
          (check-result str (take-while #'evenp lst)))))))

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
      (:ident '(:ident "hello")) :comma
      (:integer 234) :comma
      (:ident '(:ident "there")) :comma
      (:integer 5) :pipe :colon
      (:ident '(:ident "a")) :close-brace))
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

(test test-pascal-lexer-locations
  (check-lexer-locations prs/e:small-pascal-lexer
    ("
type smallstring = packed array[1..20] of char;
     record a, b, c : sometype ;
            other: packed array [ 4 .. 35 ] of char;"
     (:type 2 1)
     (:ident 2 6)
     (:equals 2 18)
     (:packed 2 20)
     (:array 2 27)
     (:open-brace 2 32)
     (:integer 2 33)
     (:range-dots 2 34)
     (:integer 2 36)
     (:close-brace 2 38)
     (:of 2 40)
     (:ident 2 43)
     (:semicolon 2 47)

     (:record 3 6)
     (:ident 3 13)
     (:comma 3 14)
     (:ident 3 16)
     (:comma 3 17)
     (:ident 3 19)
     (:colon 3 21)
     (:ident 3 23)
     (:semicolon 3 32)

     (:ident 4 13)
     (:colon 4 18)
     (:packed 4 20)
     (:array 4 27)
     (:open-brace 4 33)
     (:integer 4 35)
     (:range-dots 4 37)
     (:integer 4 40)
     (:close-brace 4 43)
     (:of 4 45)
     (:ident 4 48)
     (:semicolon 4 52))))

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

(defun gen-small-grammar-obj (&optional (max-depth 10))
  (let* ((elt (fiveam:gen-one-element
               :integer :string :ident :symbol
               :cons :list))
         (gen-number (fiveam:gen-integer :min 0 :max 100000))
         (gen-string (fiveam:gen-string
                      :elements (fiveam:gen-character
                                 :code (fiveam:gen-integer
                                        :min (char-code #\A)
                                        :max (char-code #\z)))))
         (valid-ident-character
          (lambda ()
            (let ((n (funcall (fiveam:gen-integer :min 0
                                                  :max (1- (* 2 26))))))
              (if (< n 26)
                  (code-char (+ (char-code #\a) n))
                  (code-char (+ (char-code #\A) (- n 26)))))))
         (ident (fiveam:gen-string
                 :length (fiveam:gen-integer :min 1 :max 30)
                 :elements valid-ident-character)))
    (labels ((do-gen-obj (depth)
               (ecase (funcall elt)
                 (:integer (funcall gen-number))
                 (:string (funcall gen-string))
                 (:ident (list :ident (funcall ident)))
                 (:symbol (list :symbol (funcall ident)))
                 (:cons (unless (>= depth max-depth)
                            (cons (do-gen-obj (1+ depth)) (do-gen-obj (1+ depth)))))
                 (:list (unless (>= depth max-depth)
                            (funcall (fiveam:gen-list
                                      :elements
                                      (lambda () (do-gen-obj (1+ depth))))))))))
      #'(lambda () (do-gen-obj 0)))))

(defun small-grammar-obj-to-string (obj)
  (typecase obj
    (fixnum (format nil "~s" obj))
    (string (format nil "\"~a\"" obj))
    (null "[]")
    (cons
     (case (car obj)
       (:ident (cadr obj))
       (:symbol (format nil ":~a" (cadr obj)))
       (otherwise
        (labels ((grammar-listp (obj)
                   (and (consp obj)
                        (not (eq (car obj) :ident))
                        (not (eq (car obj) :symbol))))
                 (list-to-string (obj)
                   (cond
                     ((null (cdr obj))
                      (small-grammar-obj-to-string (car obj)))
                     ((grammar-listp (cdr obj))
                      (format nil "~a,~a"
                              (small-grammar-obj-to-string (car obj))
                              (list-to-string (cdr obj))))
                     (t (format nil "~a|~a"
                                (small-grammar-obj-to-string (car obj))
                                (small-grammar-obj-to-string (cdr obj)))))))
          (format nil "[~a]" (list-to-string obj))))))
    (t (error "invalid grammar obj: ~s" obj))))

(test test-simple-grammar
  :description "Test the simple grammar"
  (check-grammar prs/e:small-grammar
    ("32" 32)
    ("123.0" 123.0)
    ("hello" '(:ident "hello"))
    ("yHmCvvFmGv" '(:ident "yHmCvvFmGv"))
    ("\"hello\"" "hello")
    ("[]" nil)
    ("[ 1,2, 3, 4 ,5]" '(1 2 3 4 5))
    ("[1|2]" '(1 . 2))
    (":hello" '(:symbol "hello"))
    ("[:hi, :there, 1, 2, [], 453, [:a|:b]|artichoke]"
     '((:symbol "hi") (:symbol "there") 1 2 () 453 ((:symbol "a") . (:symbol "b")) . (:ident "artichoke")))))

(test test-generated-simple-grammar
  :description
  (for-all ((grammar-obj (gen-small-grammar-obj)))
    (let ((as-string (small-grammar-obj-to-string grammar-obj)))
      (check-grammar prs/e:small-grammar
                     (as-string grammar-obj)))))

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