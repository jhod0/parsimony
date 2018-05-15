# Parsimony [![Build Status](https://travis-ci.org/jhod0/parsimony.svg?branch=master)](https://travis-ci.org/jhod0/parsimony)

A simple, frugal, Common Lisp library for building and using parsers, with zero external dependencies.

Parsimony's core are utilities for creating and executing monadic parsers, and includes a number of parser combinators. Parser input streams are abstracted such that an input stream could be a stream of characters, bytes, or any arbitrary Lisp type.

Built on this core are facilities for defining lexers and defining grammars. See below.

## Parsimony Core

Parsers are created via `make-parser` or `defparser`, and are used via `eval-parser`.

### Evaluating Parsers

The function `eval-parser` accepts the keywords `:input`, `:catch`, `:raise`, and `:default`.

```lisp
CL-USER> (prs:eval-parser (prs:parse-int))
1234
; => 1234

CL-USER> (with-input-from-string (s "532")
           (prs:eval-parser (prs:parse-int) :input s)
; => 532

CL-USER> (prs:eval-parser (prs:parse-int))
artichoke
; => raises a prs:parse-failure error

CL-USER> (prs:eval-parser (prs:parse-int) :raise nil)
whodunit
; => :noparse

CL-USER> (prs:eval-parser (prs:parse-int) :catch t)
mandarin
; => #(PRS:PARSE-FAILURE ...)
```

There exist a few helpful parsing macros as well, primarily `parse-loop` and `with-parsed`. `parse-loop` will continually try to parse the same thing and execute the relevent code body, and the first time the parse fails, will exit the loop. `with-parsed` does the same thing, but only once, and will raise a `parse-failure` on failure. They share a common syntax, as follows:

```lisp
(prs:with-parsed (&optional input-stream)
    ((value parser)
     &rest other-parsers)
  &rest body)

(prs:parse-loop (&optional input-stream)
    ((value parser)
     &rest other-parsers)
  &rest body)
```

Example usage:

```lisp
CL-USER> (with-input-from-string (s "a,b")
           ;; We use `s` as the input stream, and bind
           ;; `str1` and `str2` to the results of parse-alphabetical.
           (prs:with-parsed (s)
               ((str1 (parse-alphabetical))
                (:ignore (prs:parse-char #\,))
                (str2 (parse-alphabetical)))
             (values str1 str2)))
; => "a"
;    "b"

CL-USER> (prs:parse-loop ()
             ((:ignore (prs:parse-char #\*))
              (this-line (prs:parse-until (prs:parse-char #\newline))))
           (format t "~s~%" this-line))
*testing testing
(#\t #\e #\s #\t #\i #\n #\g #\  #\t #\e #\s #\t #\i #\n #\g)
*elvis has entered the building
(#\e #\l #\v #\i #\s #\  #\h #\a #\s #\  #\e #\n #\t #\e #\r #\e #\d #\  #\t #\h #\e #\  #\b #\u #\i #\l #\d #\i #\n #\g)
invalid
; => (values)
```

### Using `make-parser` and `defparser`

The macro `make-parser` constructs a parser literal, and `defparser` defines a named parser to be used in the future.

Four functions are made available inside the body of `make-parser` and `defparser`: `next`, `peek`, `recurse`, and `fail`.

1. `next` : Returns the next element in the stream and advances. Function with zero arguments.
2. `peek` : Returns the next element in the input stream, without advancing.
3. `recurse` : Applies the current parser again and returns its result. Useful for, e.g., parsing lists, look for examples in [src/combinators.lisp](src/combinators.lisp). Accepts any of the keyword arguments that go to `eval-parser`.
4. `fail` : Makes sure the parse fails, by triggering a `parse-failure` condition. Does not return. Accepts a single argument, the input which caused a failure.

Examples:

```lisp
;; Creates a parser which accepts any single character except 'a'
(prs:make-parser :my-silly-parser ()
  (let ((c (prs:next)))
    (if (not (eq c #\a))
        c
        (prs:fail c))))

;; The same could be accomplished via:
(prs:fulfills (lambda (c) (not (eq c #\a))))

;; Constructs a parser which will recognize two comma-separated integers
(prs:make-parser :my-silly-parser ((a (prs:parse-int))
                                   (:ignore (prs:parse-char #\,))
                                   (b (prs:parse-int)))
  (cons a b))


;; Parses only an even number
(prs:defparser parse-even () ((a (prs:parse-int)))
  (if (evenp a)
    (prs:fail a)
    a))
```

## Lexers and Grammars

The `deflexer` and `defgrammar` do just that - define lexers and grammars. A lexer identifies tokens in an input stream, and a grammar interprets a string of provided by a lexer. In `parsimony` both tokens and grammar targets - terminals and nonterminals, respectively, in comman `yacc` parlance - are described by keywords.

Look to `examples/grammars.lisp` for examples of both.

### Lexers

A lexer splits an incoming stream into tokens. `parsimony` tokens are defined by keywords. For a programming language, tokens are often things like `:integer`, `:open-parenthesis`, `keyword-for`. Frequently, an incoming stream is a character stream, such as a file being read, but `parsimony` lexers could accept a custom stream.

Example:

```lisp
(prs:deflexer dummy-lexer
  :documentation "Test a dummy lexer"
  ;; Only whitespace is the space character. You can use any arbitrary parser
  ;; for this - a frequent case would include tabs and newline characters.
  :whitespace (prs:parse-char #\space)
  :terminals
  (;; The first two terminals use single parsers.
   (:float (prs:parse-float))
   (:int (prs:parse-int))
   ;; Character literal
   (:newline #\newline)
   ;; String literal
   (:not-my-cabbages "Not my cabbages!!!")
   ;; Full parser - the ((name ...)) is used as an argument to `with-parsed`,
   ;; and `(coerce name 'string)` is the body of this terminal.
   (:ident ((name
             (prs:parse-some (prs:one-of "abcdefghijklmnopqrstuvwxyz"))))
           (coerce name 'string))))
```

### Grammars

TODO flesh out