(in-package :parsimony)

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


(defparameter *default-parse-input* *standard-input*
  "Default - if no parser input is specified, will use this")

(define-condition parse-failure (error)
  ((parser-name :reader parse-failure-name
                :type '(or null symbol)
                :initform nil)
   (problem-input :reader parse-failure-problem
                  :type '(or null (cons t null))
                  :initform nil)
   (cause :reader parse-failure-cause
          :type '(or null parse-failure)
          :initform nil)))

(defun parse-failure-propogate (name failure)
  (declare (type parse-failure failure)
           (type symbol name))
  (signal 'parse-failure :parser-name name :cause failure
          :problem-input (parse-failure-problem failure)))

(defun parse-failure-backtrace (failure)
  (declare (type parse-failure failure))
  (labels ((get-trace (pf)
             (cons (parse-failure-name pf)
                   (when (parse-failure-cause pf)
                     (get-trace (parse-failure-cause pf))))))
    (values (get-trace failure) (parse-failure-problem failure))))
                       
                      

(defmacro with-parse-input ((stream) &rest body)
  `(let ((*default-parse-input* ,stream))
     ,@body))


;; ======== Parser context, unwinding ========

(defstruct (parse-context (:conc-name :pc-))
  (framecount 0 :type fixnum)
  (stacks nil :type list)
  input-stream)

(defstruct (parser (:constructor make-parser-raw))
  name
  (function (error "must provide function") :type (function (parse-context parser) t)))

;; input stream -> context
(defun new-parse-context (input)
  "Constructs a new parser context which will parse the given input stream"
  (make-parse-context :framecount 0 :stacks nil
                      :input-stream input))

;; context -> frameID
(defun new-parser-frame (ctxt)
  (declare (type parse-context ctxt))
  (push (cons (incf (pc-framecount ctxt)) nil)
        (pc-stacks ctxt))
  (pc-framecount ctxt))

;; frameID context -> ?
;; unwinds all parsing up to and including the given frame
(defun unwind-until (id ctxt)
  (declare (type fixnum id)
           (type parse-context ctxt))
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
  (declare (type parse-context ctxt))
  (push obj (cdar (pc-stacks ctxt))))

(defun pop-obj (ctxt)
  "Pops an object from the parser context's stack"
  (declare (type parse-context ctxt))
  (pop (cadr (pc-stacks ctxt))))



;; ======== Parser evaluation ========

(defun eval-parser (parser
                    &key (input *default-parse-input*)
                      (raise t) (default :noparse))
  "Attempts a given parser on an input stream (or existing parser context)"
  (declare (type parser parser))
  (unless (parse-context-p input)
    (setf input (new-parse-context input)))
  (handler-case
      (funcall (parser-function parser) input parser)
    (parse-failure ()
      (if raise
          (error 'parse-failure)
          default))))

(defmacro parse-loop ((name value &rest parser-args) &rest body)
  (let ((pname (gensym)))
    `(handler-case
       (let ((,pname ,value))
         (do ((,name (eval-parser ,pname ,@parser-args)
                     (eval-parser ,pname ,@parser-args)))
           (nil)
           ,@body))
       (parse-failure () (values)))))

(defmacro make-parser (name parsers &rest body)
  "Macro which constructs a parser, with the given name, which will conduct the action given in BODY"
  `(make-parser-raw
      :name (if (symbolp ,name)
                ,name
                (error "Parser name not a symbol: ~a" ,name))
      :function
      (lambda (ctxt self)
        (declare (type parse-context ctxt))
        (let ((cur-frame (new-parser-frame ctxt))
              (input (pc-input-stream ctxt)))
          ;; Create helper functions which may be used in
          ;; the body
          (flet ((peek () (peek-stream input))
                 (next ()
                   (let ((c (get-stream input)))
                     (unless (eq c :eof)
                       (push-obj c ctxt))
                     c))
                 (fail (bad-input)
                       (error 'parse-failure :problem-input bad-input))
                 (recurse (&key (raise t) (default :noparse))
                    (eval-in-context self :raise raise :default default)))
            (declare (ignore (function peek) (function next) (function fail)))

            ;; Actually execute the parser
            (handler-case
 
             ,(if parsers
                  `(with-parsed (ctxt) ,parsers
                                ,@body)
                `(progn ,@body))
             
             ;; Clean up
             (parse-failure (err)
                            (unwind-until cur-frame ctxt)
                            (parse-failure-propogate ,name err))))))))

(defmacro defparser (name args parsers &rest body)
  `(defun ,name ,args
     (make-parser ',name ,parsers ,@body)))

(defmacro with-parsed ((&optional input)
                       ((pattern parser &rest args) &rest parses)
                       &rest forms)
  "Evaluates an arbitrary number of parsers in series, and binds them to variables

Example:

(with-parsed (input-stream) ;; custom input is optional - use ()
    ((number (parse-int))
     (:ignore (whitespace))
     (number2 (parse-int)))
    (cons number number2))"
  (let ((continuation
         (if parses
             (list `(with-parsed ,(when input (list input))
                      (,@parses)
                      ,@forms))
             forms))
        (parse `(eval-parser ,parser ,@(when input `(:input ,input))
                             ,@args)))
    (cond
      ((eq pattern :ignore)
        `(progn ,parse ,@continuation))
      ((symbolp pattern)
        `(let ((,pattern ,parse))
           ,@continuation))
      (t `(multiple-value-bind ,pattern ,parse
            ,@continuation)))))
  
(defmacro eval-in-context (parser &rest args)
  `(eval-parser ,parser :input ctxt ,@args))
