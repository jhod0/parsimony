
(in-package :parsimony/tests)

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
           (coerce s 'string))

   (:open-brace (prs:parse-char #\[))
   (:close-brace (prs:parse-char #\]))
   (:open-paren (prs:parse-char #\())
   (:close-paren (prs:parse-char #\)))
   (:comma (prs:parse-char #\,))
   (:pipe (prs:parse-char #\|))
   (:colon (prs:parse-char #\:))
   (:equals (prs:parse-char #\=))))

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
            (list :symbol id)))))


#|

===== Simplified Pascal Grammar =====

Pascal grammar examples taken from:
http://www.cs.utsa.edu/~wagner/CS3723/grammar/examples2.html

|#

(prs:deflexer small-pascal-lexer
  :documentation "Lexer for subset of Pascal"
  :whitespace (prs:alternative (prs:parse-char #\space)
                               (prs:parse-char #\newline))
  :terminals
  ((:type (prs:expect-string "type"))
   (:var (prs:expect-string "var"))
   (:record (prs:expect-string "record"))
   (:end (prs:expect-string "end"))
   (:packed (prs:expect-string "packed"))
   (:array (prs:expect-string "array"))
   (:of (prs:expect-string "of"))

   (:ident ((s (prs:parse-some (prs:one-of "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))))
           (coerce s 'string))
   (:integer (prs:parse-int))

   (:colon (prs:parse-char #\:))
   (:semicolon (prs:parse-char #\;))
   (:comma (prs:parse-char #\,))
   (:equals (prs:parse-char #\=))
   (:carrot (prs:parse-char #\^))
   (:range-dots (prs:expect-string ".."))

   (:open-brace (prs:parse-char #\[))
   (:close-brace (prs:parse-char #\]))
   (:open-paren (prs:parse-char #\())
   (:close-paren (prs:parse-char #\)))))

(prs:defgrammar small-pascal
  :lexer small-pascal-lexer
  :rules
  ((:toplevel :vardecl :typedecl)

   (:vardecl
    ((:var (:vardecllist lst) :semicolon)
     (cons :vars lst)))

   (:vardecllist
    (((:varandtype vt) :semicolon (:vardecllist rst))
     (cons vt rst))
    (((:varandtype vt))
     (list vt)))

   (:varandtype
    (((:idlist names) :colon (:typespec ty))
     (list :var :idents names :type ty)))

   (:typedecl
    ((:type (:typedeflist lst)) lst))

   (:typedeflist
    (((:typedef def) (:typedeflist lst))
     (cons def lst))
    (((:typedef def))
     (list def)))

   (:typedef
    (((:typeid id) :equals (:typespec spec) :semicolon)
     (list :typedef id spec)))

   (:typespec :typeid :arraydef :ptrdef :rangedef :enumdef :recdef)

   (:typeid :ident)

   (:arraydef
    ((:packed :array :open-brace (:rangedef rd) :close-brace :of (:typeid id))
     (list :arraydef :packed t :range rd :of id))
    ((:array :open-brace (:rangedef rd) :close-brace :of (:typeid id))
     (list :arraydef :packed nil :range rd :of id)))

   (:ptrdef
    ((:carrot (:typeid id)) (list :ptr id)))

   (:rangedef
    (((:integer start) :range-dots (:integer end))
     (list :range start end)))

   (:enumdef
    ((:open-paren (:idlist lst) :close-paren)
     (cons :enum lst)))

   (:idlist
    (((:ident id) :comma (:idlist rst))
     (cons id rst))
    (((:ident id))
     (list id)))

   (:recdef
    ((:record (:vardecllist vars) :end :semicolon)
     (cons :record vars)))))