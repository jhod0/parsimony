
(in-package :parsimony/examples)

(prs:deflexer hoc
  :documentation "Lexer for the `hoc` calculator from K&P"
  :terminals
  ((:number (prs:alternative
             (prs:pmap #'float (prs:parse-int))
             (prs:parse-float)))
   ))