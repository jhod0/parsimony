(in-package :parsimony/examples)


(prs:deflexer dummy-lexer
  :documentation "Test a dummy lexer"
  :whitespace (prs:parse-char #\space)
  :terminals
  ((:float (prs:parse-float))
   (:int (prs:parse-int))
   (:newline #\newline)
   (:ident ((name
             (prs:parse-some (prs:one-of "abcdefghijklmnopqrstuvwxyz"))))
           (coerce name 'string))))

(prs:deflexer small-grammar-lexer
  :documentation "Lexer for a small grammar"
  :whitespace (prs:alternative (prs:parse-char #\space)
                               (prs:parse-char #\newline))
  :terminals
  ((:float (prs:parse-float))
   (:integer (prs:parse-int))

   (:string ((:ignore (prs:parse-char #\"))
             (str (prs:parse-until (prs:parse-char #\"))))
            (coerce str 'string))
   (:ident ((s (prs:parse-some (prs:one-of "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))))
           (list :ident (coerce s 'string)))

   (:open-brace #\[)
   (:close-brace #\])
   (:open-paren  #\()
   (:close-paren #\))
   (:comma #\,)
   (:pipe #\|)
   (:colon #\:)
   (:equals #\=)))

(prs:defgrammar small-grammar
  :description "A grammar for parsing simple objects, with elixir-like syntax"
  :lexer small-grammar-lexer
  :rules (
          ;; An obj is either an integer, float, string, ident, or list
          (:obj :integer :float :string :ident :list :symbol)

          ;; objs - list of obj separated by commas
          (:objs
           (((:obj o) :comma (:objs os))
            (cons o os))
           (((:obj o))
            (list o))
           (() nil))

          ;; List - list of obj, enclosed in [], comma-separated
          (:list
           ((:open-brace (:objs lst) :close-brace) lst)
           ((:open-brace (:objs lst) :pipe (:obj last) :close-brace)
            (labels ((make-dotted (l)
                                  (if l
                                      (cons (car l) (make-dotted (cdr l)))
                                    last)))
              (make-dotted lst))))

          ;; Symbol - like Lisp keywords, a colon followed by an identifier
          (:symbol
           ((:colon (:ident id))
            (list :symbol (cadr id))))))


(prs:deflexer json-lexer
  :documentation "Lexer for simplified JSON input (no string escapes)"
  :whitespace (prs:alternative (prs:parse-char #\space)
                               (prs:parse-char #\newline))
  :terminals
  ((:jsonint (prs:parse-int))

   (:jsonstring
    ((:ignore (prs:parse-char #\"))
     (str (prs:while-fulfills (lambda (c) (not (eq c #\"))))))
    (coerce str 'string))

   (:null ((:ignore (prs:expect-string "null")))
          :null)

   (:open-curly #\{)
   (:close-curly #\})
   (:open-brace #\[)
   (:close-brace #\])
   (:colon #\:)
   (:comma #\,)))

(prs:defgrammar json-parser
    :description "Parses a subset of JSON"
    :lexer json-lexer
    :rules
    ((:jsonobj :jsondict :jsonlist :jsonlit)

     (:jsondict
      ((:open-curly (:dict-innards di) :close-curly)
       di)
      ((:open-curly :close-curly) nil))

     (:jsonlist
      ((:open-brace (:jsonobjs jo) :close-brace)
       jo))

     (:dict-innards
      (((:jsonstring s) :colon (:jsonobj obj) :comma (:dict-innards rest))
       (cons (cons s obj) rest))
      (((:jsonstring s) :colon (:jsonobj obj))
       (list (cons s obj))
       ))

     (:jsonobjs
      (((:jsonobj obj) (:jsonobjs-rest objs))
       (cons obj objs))
      (((:jsonobj obj))
       (list obj))
      (() nil))

     (:jsonobjs-rest
      ((:comma (:jsonobjs os))
       os)
      (() nil))

     (:jsonlit :jsonstring :jsonint :null)))