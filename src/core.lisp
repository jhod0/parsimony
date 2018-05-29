(in-package :parsimony/core)

(defgeneric get-stream (stream ctxt)
  (:documentation "Take an object from a stream. Like gray streams, yields object or :eof."))

(defgeneric put-stream (obj stream)
  (:documentation "Replace an object in a stream."))

(defgeneric peek-stream (stream ctxt)
  (:documentation "Yields an object, without moving the stream forward."))

(defgeneric stream-location (stream)
  (:documentation "The location of the next object in the stream, i.e., file, line, col no."))



(defparameter *default-parse-input* *standard-input*
  "Default - if no parser input is specified, will use this")

(define-condition parse-failure (error)
  ((parser :accessor parse-failure-parser
           :type parser
           :initarg :parser
           :initform nil)
   (location :accessor parse-failure-location
             :initarg :location
             :initform nil)
   (problem-input :accessor parse-failure-problem
                  :type '(or null (cons t null))
                  :initarg :problem-input
                  :initform nil)
   (cause :accessor parse-failure-cause
          :type '(or null parse-failure)
          :initarg :cause
          :initform nil)))

(defun parse-failure-propogate (parser failure &optional loc)
  (declare (type parse-failure failure)
           (type parser parser))
  (error 'parse-failure :parser parser :location loc
         :cause failure
         :problem-input (parse-failure-problem failure)))

(defun parse-failure-backtrace (failure)
  (declare (type parse-failure failure))
  (labels ((get-trace (pf)
             (with-slots (cause) pf
               (cons pf
                     (when cause
                       (get-trace cause))))))
    (values (get-trace failure) (parse-failure-problem failure))))

(defun print-parse-failure-backtrace (failure &optional (out-stream *error-output*))
  (declare (type parse-failure failure))
  (let ((bt (parse-failure-backtrace failure))
        (ind 0))
    (dolist (f bt)
      (incf ind)
      (with-slots (location parser cause problem-input) f

        (format out-stream "~d:" ind)
        (when location
          (format out-stream "~aat ~a~%" #\tab location))
        (format out-stream "~ain ~a~%" #\tab parser)
        (unless cause
          (format out-stream "~aon input ~a~%" #\tab problem-input))))))


(defmacro with-parse-input ((stream) &rest body)
  `(let ((*default-parse-input* ,stream))
     ,@body))


;; ======== Parser context, unwinding ========

(defstruct (parse-context (:conc-name :pc-))
  (framecount 0 :type fixnum)
  (stacks nil :type list)
  input-stream)

(defstruct (parser (:constructor make-parser-raw))
  (name nil)
  (args nil)
  (function (error "must provide function") :type (function (parse-context parser) t)))

(defmethod print-object ((obj parser) stream)
  (if (parser-args obj)
      (format stream "#<PRS:PARSER named: ~a args: ~a>"
              (parser-name obj) (parser-args obj))
    (format stream "#<PRS:PARSER named: ~a>" (parser-name obj))))

;; input stream -> context
(declaim (inline new-parse-context))
(defun new-parse-context (input)
  "Constructs a new parser context which will parse the given input stream"
  (make-parse-context :framecount 0 :stacks nil
                      :input-stream input))

;; context -> frameID
(declaim (inline new-parser-frame))
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
             (decf (pc-framecount ctxt))
             (dolist (o (cdar lst))
               (put-stream o (pc-input-stream ctxt)))
             (if (eq (caar lst) id)
                (cdr lst)
                (do-unwind (cdr lst)))))
    (setf (pc-stacks ctxt)
          (do-unwind (pc-stacks ctxt)))))

(defun push-obj-to-context (obj ctxt)
  "Pushes an object to the parser context's stack"
  (declare (type parse-context ctxt))
  (push obj (cdar (pc-stacks ctxt))))

(defun pop-obj-from-context (ctxt)
  "Pops an object from the parser context's stack"
  (declare (type parse-context ctxt))
  (pop (cadr (pc-stacks ctxt))))


;; === Define methods for basic streams ===

(defmethod get-stream ((s stream) (ctxt null))
  (declare (ignore ctxt))
  (read-char s nil :eof))

(defmethod get-stream ((s stream) (ctxt parse-context))
  (let ((c (get-stream s nil)))
    (unless (eq c :eof)
      (push-obj-to-context c ctxt))
    c))

(defmethod put-stream (obj (s stream))
  (unread-char obj s))

(defmethod peek-stream ((s stream) ctxt)
  (peek-char nil s nil :eof))

(defmethod stream-location ((s stream))
  nil)


;; ======== Parser evaluation ========

(defun %do-eval-parser (parser
                        &key (input *default-parse-input*)
                          catch-failure (raise t) (default :noparse))
  "Attempts a given parser on an input stream (or existing parser context)"
  (declare (type parser parser)
           (type boolean raise)
           (type boolean catch-failure))
  (unless (parse-context-p input)
    (setf input (new-parse-context input)))
  (handler-case
   (funcall (parser-function parser) input parser)
   (parse-failure (err)
      (cond
       (catch-failure err)
       (raise (error err))
       (t default)))))

(defun eval-parser (parser
                    &key (input *default-parse-input*)
                      catch-failure (raise t) (default :noparse))
  (handler-case
      (with-input-from-string (s (if (stringp input)
                                     input
                                     ""))
          (%do-eval-parser parser
                           :input (if (stringp input) s input)
                           :catch-failure catch-failure
                           :raise raise :default default))
    (parse-failure (err)
      (restart-case
          (error err)
        (print-backtrace ()
          :report "Print a backtrace of the parser failure."
          (print-parse-failure-backtrace err))
        (print-backtrace-reraise ()
          :report "Print a backtrace, and reraise the failure."
          (print-parse-failure-backtrace err)
          (error err))))))

(defmacro parse-loop ((&optional context) parsers &rest body)
  `(handler-case
    (loop do
          (with-parsed ,(when context (list context))
                       ,parsers
                       ,@body))
     (parse-failure () (values))))

(defmacro parse-loop-in-context (parsers &rest body)
  `(parse-loop (ctxt) ,parsers ,@body))

(defmacro save-stream-position (&rest body)
  "Saves the current parser stack position, and when BODY is finished executing
restores the input stream to the position it was before BODY"
  (let ((tmp-frame (gensym "save-frame")))
    `(let ((,tmp-frame (new-parser-frame ctxt)))
       (unwind-protect
            (progn ,@body)
         (unwind-until ,tmp-frame ctxt)))))

(defmacro make-parser (name parsers &rest body)
  "Macro which constructs a parser, with the given name, which will conduct the action given in BODY"
  `(make-parser-raw
      :name (if (symbolp ,name)
                ,name
                (error "Parser name not a symbol: ~a" ,name))
      :function
      (lambda (ctxt self)
        (declare (type parse-context ctxt)
                 (type parser self))
        (let* ((%cur-frame% (new-parser-frame ctxt))
               (%input% (pc-input-stream ctxt))
               (%starting-location% (stream-location %input%))
               (%recent-location% %starting-location%))
          (declare (type fixnum %cur-frame%))
          ;; Create helper functions which may be used in
          ;; the body
          (flet ((peek () (peek-stream %input% ctxt))
                 (next ()
                   (setf %recent-location% (stream-location %input%))
                   (get-stream %input% ctxt))
                 (location () (stream-location %input%))
                 (fail (bad-input)
                   (error 'parse-failure
                          :parser self
                          :location %recent-location%
                          :problem-input bad-input))
                 (recurse (&key (raise t) (default :noparse))
                    (declare (type boolean raise))
                    (eval-in-context self :raise raise :default default)))

            (declare (ignore (function peek) (function next)
                             (function location) (function fail)
                             (function recurse))
                     (inline peek next
                             location fail))

            ;; Actually execute the parser
            (handler-case

             ,(if parsers
                  `(with-parsed (ctxt) ,parsers
                                ,@body)
                `(progn ,@body))

             ;; Clean up
             (parse-failure (err)
               (unwind-until %cur-frame% ctxt)
               (assert (or (not (eq (parse-failure-location err)
                                    %starting-location%))
                           (eq %starting-location% %recent-location%)))
               (parse-failure-propogate self err %starting-location%))))))))

(defmacro defparser (name args parsers &rest body)
  (let* ((tmp (gensym))
         docstring)
    (when (stringp (car body))
      (setf docstring (pop body)))
    `(defun ,name ,args
       ,@(when docstring (list docstring))
       (let ((,tmp (make-parser ',name ,parsers ,@body)))
         (setf (parser-args ,tmp)
               (list ,@(remove-if (lambda (arg)
                                    (or (not (symbolp arg))
                                        (member arg '(&optional &key &rest))))
                                  args)))
         ,tmp))))


(defmacro with-parsed ((&optional input)
                       ((pattern parser &rest args) &rest parses)
                       &rest forms)
  "Evaluates an arbitrary number of parsers in series, and binds them to variables

Example:

(with-parsed (input-stream) ;; custom input is optional - use ()
    ((number (parse-int))
     (:ignore (whitespace))
     (number2 (parse-int)))
    (cons number number2))
"

  (let ((continuation
         (if parses
             (list `(with-parsed ,(when input (list input))
                      (,@parses)
                      ,@forms))
             forms))
        (parse
          (cond
            ((eq parser :location) '(location))
            ((eq parser :next) '(next))
            (t `(%do-eval-parser ,parser ,@(when input `(:input ,input))
                                 ,@args)))))
    (cond
      ((eq pattern :ignore)
        `(progn ,parse ,@continuation))
      ((symbolp pattern)
        `(let ((,pattern ,parse))
           ,@continuation))
      (t `(multiple-value-bind ,pattern ,parse
            ,@continuation)))))

(defmacro eval-in-context (parser &rest args)
  `(%do-eval-parser ,parser :input ctxt ,@args))