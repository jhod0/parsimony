
(in-package :parsimmons)

(defgeneric get-stream (stream)
            (:documentation "Take an object from a stream. Like gray streams, yields object or :eof"))

;; is this one necessary?
(defgeneric put-stream (obj stream)
            (:documentation "Replace an object in a stream"))

(defgeneric peek-stream (stream)
            (:documentation "Yields an object, withoug moving the stream forward."))



(defmethod get-stream ((s stream))
  (read-char s nil :eof))

(defmethod put-stream (obj (s stream))
  (unread-char obj s))

(defmethod peek-stream ((s stream))
  (peek-char nil s nil :eof))

(defparameter *default-parse-input* *standard-input*)

(define-condition parse-failure (error) ())

(defmacro with-parse-input ((stream) &rest body)
  `(let ((*default-parse-input* ,stream))
     ,@body))


;; ======== Parser context, unwinding ========

(defstruct (parse-context (:conc-name :pc-))
  framecount
  stacks
  input-stream)

;; input stream -> context
(defun new-parse-context (input)
  "Constructs a new parser context which will parse the given input stream"
  (make-parse-context :framecount 0 :stacks nil
		      :input-stream input))

;; context -> frameID
(defun new-parser-frame (ctxt)
  (push (cons (incf (pc-framecount ctxt)) nil)
        (pc-stacks ctxt))
  (pc-framecount ctxt))

;; frameID context -> ?
;; unwinds all parsing up to and including the given frame
(defun unwind-until (id ctxt)
  (labels ((do-unwind (lst)
              (dolist (o (cdar lst))
                (put-stream o (pc-input-stream ctxt)))
              (if (eq (caar lst) id)
                (cdr lst)
                (do-unwind (cdr lst)))))
    (setf (pc-stacks ctxt)
          (do-unwind (pc-stacks ctxt)))))

(defun push-obj (obj ctxt)
  "Pushes an object to the parser context's stack"
  (push obj (cdar (pc-stacks ctxt))))

(defun pop-obj (ctxt)
  "Pops an object from the parser context's stack"
  (pop (cadr (pc-stacks ctxt))))



;; ======== Parser evaluation ========

(defun eval-parser (parser
   &optional (ctxt-or-stream *default-parse-input*)
		    &key (raise t) (default :noparse))
  "Attempts a given parser on an input stream (or existing parser context)"
  (unless (parse-context-p ctxt-or-stream)
    (setf ctxt-or-stream (new-parse-context ctxt-or-stream)))
  (handler-case
      (funcall parser ctxt-or-stream)
    (parse-failure ()
      (if raise
	  (error 'parse-failure)
	  default))))

(defmacro parse-loop ((name value &rest parser-args) &rest body)
  (let ((pname (gensym))
        (errname (gensym)))
    `(handler-case
       (let ((,pname ,value))
         (do ((,name (eval-parser ,pname ,@parser-args)
                     (eval-parser ,pname ,@parser-args)))
           (nil)
           ,@body))
       (parse-failure (,errname) ,errname))))

(defmacro make-parser (&rest body)
  "Macro which constructs a parser, which will conduct the actiond given in BODY"
  `(lambda (ctxt)
     (let ((cur-frame (new-parser-frame ctxt))
	   (input (pc-input-stream ctxt)))
       (flet ((peek () (peek-stream input))
	      (next ()
		    (let ((c (get-stream input)))
		      (unless (eq c :eof)
		        (push-obj c ctxt))
		      c))
	      (fail (&rest args)
		(apply #'error
		       (cons 'parse-failure args))))
	 (handler-case
	     
	     (progn ,@body)
	   
	   (parse-failure (err)
	     (unwind-until cur-frame ctxt)
	     (error err)))))))

(defmacro defparser (name args &rest body)
  `(defun ,name ,args
     (make-parser ,@body)))

(defmacro eval-in-context (parser &rest args)
  `(eval-parser ,parser ctxt ,@args))

;; ======= Parser Combinators =======

(defparser alternative (&rest parsers)
  (block alt
    (dolist (p parsers)
      (let ((v (eval-in-context p)))
        (unless (eq v :noparse)
          (return-from alt v))))
    (fail)))

(defparser parse-all (&rest parsers)
  (mapcar #'(lambda (parser) (eval-in-context parser))
          parsers))

(defparser parse-some (parser)
  (let ((fst (eval-in-context parser)))
    (cons fst (eval-in-context (parse-many parser)))))

(defparser parse-many (parser)
  (let ((s (parse-some parser)))
    (eval-in-context s :raise nil :default nil)))


;; ======== Example parsers ========

(defun digitp (c)
  (member c (coerce "0123456789" 'list)))

(defparser parse-digit ()
  (if (digitp (peek))
    (- (char-int (next)) (char-int #\0))
    (fail)))

(defparser parse-int ()
  (let ((p (parse-digit)))
    (do ((d (eval-in-context p) (eval-in-context p :raise nil))
         (n 0))
      ((eq d :noparse) n)
      (setf n (* n 10))
      (incf n d))))

(defparser parse-char (c)
  (if (eq (next) c)
    c (fail)))

(defparser parse-float ()
  (let (big little)
    (setf big (eval-in-context (parse-int)))
    (eval-in-context (parse-char #\.))
    (setf little (eval-in-context (parse-int)))
    (cons big little)))

(defparser one-of (lst)
  (let ((c (next)))
    (if (member c (coerce lst 'list))
      c
      (fail))))





;; ======== Testing ========

(defun whitespace ()
  (one-of '(#\space #\tab #\newline)))

(defvar numbers (parse-some (parse-all (parse-many (whitespace))
                                       (parse-int))))

(defvar alpha (one-of "abcdefghijklmnopqrstuvwxyz"))
(defvar alphas (parse-some alpha))

(defvar almost-same
  (alternative (parse-all (parse-some (one-of "abc"))
                          (parse-int)
                          (parse-char #\k)
                          (whitespace))
               (parse-all (parse-some (one-of "abc"))
                          (parse-digit)
                          (one-of "zdef")
                          (whitespace))))


(defun test ()
  (parse-loop (obj almost-same)
     (format t "found another: ~s~%" obj))

  (handler-case
    (progn
      (format t "~a~%" (eval-parser almost-same)))
    (parse-failure (a)
     (format t "toplevel parse failure: ~a~%" a))))

(eval-when (:execute)
  (test))
