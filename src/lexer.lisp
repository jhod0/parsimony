
(in-package :parsimony)


(defstruct (lexer (:constructor make-lexer-struct))
  (name (error "name required") :type symbol)
  (parser (error "parser required") :type parser)
  (terminals (error "lexer needs terminals") :type list)
  (documentation nil :type (or string null)))

(defun make-lexer-name (lexer-name rule-name)
  (intern (concatenate 'string "LEX-" (symbol-name lexer-name) "-" (symbol-name rule-name))
          *package*))

(defmacro deflexer (name &key documentation whitespace terminals)
  (let* ((terminal-names (mapcar #'(lambda (n) (the keyword (car n)))
                                 terminals))
         (lexer-names (mapcar #'(lambda (term) (make-lexer-name name term))
                              terminal-names))
         (parser-core `(alternative
                        ,@(mapcar #'(lambda (name) `(,name))
                                  lexer-names))))
    `(eval-when (:compile-toplevel :load-toplevel)
       (labels
           ,(mapcar #'(lambda (rule)
                        (let ((this-name (make-lexer-name name (car rule))))
                          `(,this-name ()
                             (make-parser ',this-name
                               ,@(if (cddr rule)
                                     `(,(cadr rule)
                                       (values ,(car rule) (progn ,@(cddr rule))))
                                   (let ((tmp (gensym)))
                                     `(((,tmp ,(cadr rule)))
                                       (values ,(car rule) ,tmp))))))))
                    terminals)
         (defparameter ,name
           (make-lexer-struct :name ',name
             :documentation ,documentation
             :terminals (list ,@terminal-names)
             :parser
             ,(if whitespace
                  `(make-parser ',name ((:ignore (parse-many ,whitespace)))
                     (eval-in-context
                      ,parser-core))
                parser-core)))))))

(defmacro lex (name &rest args)
  `(eval-parser (lexer-parser ,name) ,@args))

(defun get-lexer-parser (lexer)
  (lexer-parser lexer))

(defstruct (lexer-stream (:constructor make-lexer-stream-raw))
  (lexer (error "must refer to lexer") :type lexer)
  (parser (error "must supply parser") :type parser)
  (input-stream (error "need an input stream"))
  (peeks nil :type list))

(defun lexer-stream (l &key (input *default-parse-input*))
  (declare (type lexer l))
  (make-lexer-stream-raw :lexer l
                         :parser (lexer-parser l)
                         :input-stream input))

(defmethod get-stream ((s lexer-stream) (ctxt null))
  (declare (ignore ctxt))
  (with-slots (peeks input-stream parser) s
    (if peeks
        (let ((top (pop peeks)))
          (values (car top) (cdr top)))
      (with-parsed (input-stream)
                   (((tok val) parser))
                   (values tok val)))))

(defmethod get-stream ((s lexer-stream) (ctxt parse-context))
  (multiple-value-bind (tok val) (get-stream s nil)
    (push-obj (cons tok val) ctxt)
    (values tok val)))

(defmethod put-stream (obj (s lexer-stream))
  (push obj (lexer-stream-peeks s)))

(defmethod peek-stream ((s lexer-stream) ctxt)
  (with-slots (peeks) s
    (if peeks
        (values (caar peeks) (cdar peeks))
      (multiple-value-bind (tok val) (get-stream s ctxt)
        (push (cons tok val) peeks)
        (values tok val)))))
