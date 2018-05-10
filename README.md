# Parsimony [![Build Status](https://travis-ci.org/jhod0/parsimony.svg?branch=master)](https://travis-ci.org/jhod0/parsimony)

A simple, frugal, Common Lisp library for building and using parsers, with zero external dependencies.

Parsimony's core are utilities for creating and executing monadic parsers, and includes a number of parser combinators. Parser input streams are abstracted such that an input stream could be a stream of characters, bytes, or any arbitrary Lisp type.

Built on this core are facilities for defining lexers and defining grammars. See below.

## Parsimony Core

Parsers are created via `make-parser` or `defparser`, and are used via `eval-parser`.

### Evaluating Parsers

The function `eval-parser` accepts the keywords `:input`, `:catcch`, `:raise`, and `:default`.

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

Four functions are made available inside the body of `make-parser` and `defparser`: `next`, `peek`, `recurse`, and `fail`. The first two accept zero arguments, and yield the next object in the input stream, the third attempts the parser again and returns its result, and the last accepts a single argument and triggers a `parse-failure` with its argument as the cause of failure.

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

Look to `examples/grammars.lisp` for examples.

TODO flesh out
