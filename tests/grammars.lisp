
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
  :rules ((:obj
           (((:integer i)) i)
           (((:float f)) f)
           (((:string s)) s)
           (((:ident i)) i)
           (((:list l)) l))

          (:objs
           (((:obj o) :comma (:obj os))
            (cons o os))
           (((:obj o)) (list o)))

          (:list
           ((:open-brace :close-brace) nil)
           ((:open-brace (:objs lst) :close-brace) lst)
           ((:open-brace (:objs lst) :pipe (:obj last) :close-brace)
            (labels ((make-dotted (l)
                                  (if l
                                      (cons (car l) (make-dotted (cdr l)))
                                    last)))
              (make-dotted lst))))

          (:symbol
           ((:colon (:ident id))
            id))))


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
;  :documentation "Subset of Pascal"
  :lexer small-pascal-lexer
  :rules
  ((:vardecl
    ((:var (:vardecllist lst) :semicolon)
     (cons :vars lst)))

   (:vardecllist
    (((:varandtype vt) :semicolon (:vardecllist rst))
     (cons vt rst))
    (((:varandtype vt))
     (list vt)))

   (:varandtype
    (((:varnames names) :colon (:typespec ty))
     (list :var :idents names :type ty)))

   (:varnames
    (((:ident i) (:varnames is))
     (cons i is))
    (((:ident i))
     (list i)))

   (:typedecl
    ((:type (:typedeflist lst)) lst))

   (:typedeflist
    (((:typedef def) (:typedeflist lst))
     (cons def lst))
    (((:typedef def))
     (list def)))

   (:typedef
    (((:typeid id) :equals (:typespec spec) :colon)
     (list :typedef id spec)))

   (:typespec :typeid :arraydef :ptrdef :rangedef :enumdef :recdef)

   (:typeid :ident)

   (:arraydef
    ((:packed :array :open-brace (:rangedef rd) :close-brace :of (:typeid id))
     (list :arraydef :packed t :range rd :of id))
    ((:array :open-brace (:rangedef rd) :close-brace :of (:typeid id))
     (list :arraydef :packed nil :range rd :of id)))

   (:ptrdef
    ((:carrot (:typeid id)) id))

   (:rangedef
    (((:integer start) :range-dots (:integer end))
     (list :range start end)))

   (:enumdef
    ((:open-paren (:idlist lst) :close-paren)
     (cons :enum lst)))
   (:idlist
    (((:ident id) (:idlist rst))
     (cons id rst))
    (((:ident id))
     (list id)))

   (:recdef
    ((:record (:vardecllist vars) :end :semicolon)
     (cons :record vars)))))