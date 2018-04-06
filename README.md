# Parsimony

A simple, frugal, Common Lisp library for building and using parsers, with zero external dependencies.

Parsimony's core are utilities for creating and executing monadic parsers, and includes a number of parser combinators. Parser input streams are abstracted such that an input stream could be a stream of characters, bytes, or any arbitrary Lisp type.

Built on this core are facilities for defining lexers and defining grammars. See below.

## Parsimony Core

Parsers are created via `make-parser` or `defparser`, and are used via `eval-parser`.

### Evaluating Parsers

```lisp
CL-USER> (prs:eval-parser (prs:parse-int))
1234
; => 1234

CL-USER> (prs:eval-parser (prs:parse-int))
artichoke
; => raises a prs:parse-failure error

CL-USER> (with-input-from-string (s "532") (prs:eval-parser (prs:parse-int) :input s)
; => 532
```

### Using `make-parser` and `defparser`

## Lexers and Grammars

Look to `examples/grammars.lisp` for examples.

TODO flesh out