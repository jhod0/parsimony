
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
   (:record (prs:expect-string "record"))
   (:end (prs:expect-string "end"))
   (:packed (prs:expect-string "packed"))
   (:array (prs:expect-string "array"))
   (:of (prs:expect-string "of"))

   (:ident ((s (prs:parse-some (prs:one-of "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))))
           (coerce s 'string))
   (:integer (prs:parse-int))

   (:semicolon (prs:parse-char #\;))
   (:comma (prs:parse-char #\,))
   (:equals (prs:parse-char #\=))
   (:carrot (prs:parse-char #\^))
   (:range-dots (prs:expect-string ".."))

   (:open-brace (prs:parse-char #\[))
   (:close-brace (prs:parse-char #\]))
   (:open-paren (prs:parse-char #\())
   (:close-paren (prs:parse-char #\)))))
