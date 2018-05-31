(in-package :parsimony/examples)


#|

===== Simplified Pascal Grammar =====

Pascal grammar examples taken from:
http://www.cs.utsa.edu/~wagner/CS3723/grammar/examples2.html

|#

(prs:defparser pascal-whitespace () ()
  (let ((c (next)))
    (case c
      (#\space #\space)
      (#\tab #\tab)
      (#\newline #\newline)
      (#\/
       (let ((c (next)))
         (case c
           (#\/ (prs:parse-until (prs:parse-char #\newline)))
           (otherwise (fail c)))))
      (#\(
       (let ((c (next)))
         (case c
           (#\* (prs:parse-until (prs:expect-string "*)")))
           (otherwise (fail c)))))
      (otherwise (prs:fail c)))))


(prs:deflexer small-pascal-lexer
  :documentation "Lexer for subset of Pascal"
  :whitespace (pascal-whitespace)
  :terminals
  ((:program "program")
   (:begin "begin")
   (:end "end")
   (:if "if")
   (:then "then")
   (:else "else")
   (:while "while")
   (:do "do")
   (:repeat "repeat")
   (:until "until")
   (:with "with")

   (:label "label")
   (:const "const")

   (:to "to")
   (:downto "downto")

   (:case "case")
   (:of "of")

   (:type "type")
   (:var "var")
   (:record "record")
   (:packed "packed")
   (:array "array")
   (:set "set")
   (:file "file")

   (:nil "nil")
   (:or "or")
   (:and "and")
   (:div "div")
   (:mod "mod")

   (:not "not")

   (:ident ((s (prs:parse-some (prs:one-of "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))))
           (coerce s 'string))
   (:integer (prs:parse-int))

   (:equals "=")
   (:not-equals "<>")
   (:less-than "<")
   (:less-than-or-equal "<=")
   (:greater-than ">")
   (:greater-than-or-equal ">=")

   (:plus #\+)
   (:minus #\-)
   (:times #\*)
   (:divide #\/)

   (:colon #\:)
   (:semicolon #\;)
   (:comma #\,)
   (:carrot #\^)
   (:range-dots "..")

   (:single-quote #\')
   (:double-quote #\")

   (:open-brace #\[)
   (:close-brace #\])
   (:open-paren #\()
   (:close-paren #\))))

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
    ((:type (:typedeflist lst)) (cons :typedefs lst)))

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
    ((:record (:vardecllist vars) :end)
     (cons :recdef vars)))


   ;; Expression definitions

   (:sign :plus :minus)
   (:variable :ident)

   (:expression
    (((:simple-expression se1) (:relational-operator re) (:simple-expression se2))
     (list re se1 se2))
    (((:simple-expression se))
     se))

   (:simple-expression
    (((:sign si) (:term tr) (:addition-operator at) (:term te))
     (list si at tr te))
    (((:term tr) (:addition-operator at) (:term te))
     (list at tr te))
    (((:sign si) (:term te))
     (list :term te :sign si))
    (((:term tr))
     (list :term tr)))

   (:term
    (((:factor f) (:multiplication-operator mf) (:factor fa))
     (list mf f fa))
    (((:factor f))
     f))

   (:factor
    :variable :integer #| :string :set |# :nil
    :ident
    ((:open-paren (:expression e) :close-paren)
     e)
    ((:not (:factor f))
     (list :not f)))

   (:relational-operator
    :equals :not-equals
    :less-than
    :less-than-or-equal
    :greater-than
    :greater-than-or-equal)

   (:addition-operator
    :plus :minus :or)

   (:multiplication-operator
    :times :divide :div :mod :and)))