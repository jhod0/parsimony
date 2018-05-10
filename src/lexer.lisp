
(in-package :parsimony)


(defstruct (lexer (:constructor make-lexer-struct))
  (name (error "name required") :type symbol)
  (parser (error "parser required") :type parser)
  (terminals (error "lexer needs terminals") :type list)
  (documentation nil :type (or string null)))

(defun make-lexer-name (lexer-name rule-name)
  (intern (concatenate 'string "LEX-" (symbol-name lexer-name) "-" (symbol-name rule-name))
          *package*))

(defun make-lexer-for-terminal (lexer-name rule)
  (let* ((this-name (make-lexer-name lexer-name (car rule)))
         (rule-name (car rule))
         (tmp-name (gensym))
         (res-name (gensym))
         (loc-name (gensym))
         (header (cons (list loc-name :location)
                       (if (cddr rule)
                           (cadr rule)
                           `((,tmp-name ,(cadr rule)))))))
    `(,this-name ()
       (make-parser ',this-name ,header
         (let ((,res-name ,(if (cddr rule)
                               `(progn ,@(cddr rule))
                               tmp-name)))
           (values ,rule-name ,res-name ,loc-name))))))

(defun lexer-literal-p (obj)
  "Lexer definitions can use string or character literals."
  (or (characterp obj)
      (stringp obj)))

(defun partition-lexer-rules (rules)
  "Separate lexer rule which are just literals, from more complexlexer rules.

Returns (values literal-rules complex-rules)"
  (when (null rules)
    (return-from partition-lexer-rules (values nil nil)))

  (let* ((this-rule (car rules))
         (rule-body (cdr this-rule))
         (rest-rules (cdr rules)))
    (multiple-value-bind
          (literals full-rules)
        (partition-lexer-rules rest-rules)
      (if (and (= (length rule-body) 1)
               (lexer-literal-p (car rule-body)))
          (values (cons this-rule literals)
                  full-rules)
          (values literals
                  (cons this-rule full-rules))))))

(defun make-lexer-terminal-definitions (name literals rules)
  (let ((full-rule-definitions
         (mapcar #'(lambda (rule)
                     (make-lexer-for-terminal name rule))
                 rules)))
    (if (null literals)
        full-rule-definitions
        ;; TODO implement literal parser
        (error "unimplemented"))))

(defmacro deflexer (name &key documentation whitespace terminals)
  ;; First split literals and full rules
  (multiple-value-bind (literal-rules full-rules)
      (partition-lexer-rules terminals)
    ;; Name all of the lexers, and generate the core parser expression
    (let* ((terminal-names (mapcar #'(lambda (n) (the keyword (car n)))
                                   full-rules))
           (lexer-names (mapcar #'(lambda (term) (make-lexer-name name term))
                                terminal-names))
           (literal-lexer-name (make-lexer-name name :literals))
           (parser-core `(alternative
                          ,@(when literal-rules
                                  (list `(,literal-lexer-name)))
                          ,@(mapcar #'(lambda (name) `(,name))
                                    lexer-names))))
      `(eval-when (:compile-toplevel :load-toplevel)
         (labels
             ,(make-lexer-terminal-definitions
                 name literal-rules full-rules)
           (defparameter ,name
             (make-lexer-struct :name ',name
                                :documentation ,documentation
                                :terminals (list ,@terminal-names)
                                :parser
                                ,(if whitespace
                                     `(make-parser ',name ((:ignore (parse-many ,whitespace)))
                                                   (eval-in-context
                                                    ,parser-core))
                                     parser-core))))))))

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
                         :input-stream (new-file-stream input)))

(defmethod get-stream ((s lexer-stream) (ctxt null))
  (declare (ignore ctxt))
  (with-slots (peeks input-stream parser) s
    (if peeks
        (let ((top (pop peeks)))
          (values-list top))
        (eval-parser parser :input input-stream))))

(defmethod get-stream ((s lexer-stream) (ctxt parse-context))
  (multiple-value-bind (tok val loc) (get-stream s nil)
    (push-obj (list tok val loc) ctxt)
    (values tok val loc)))

(defmethod put-stream (obj (s lexer-stream))
  (push obj (lexer-stream-peeks s)))

(defmethod peek-stream ((s lexer-stream) ctxt)
  (with-slots (peeks) s
    (if peeks
        (values-list (car peeks))
      (multiple-value-bind (tok val loc) (get-stream s ctxt)
        (push (list tok val loc) peeks)
        (values tok val loc)))))

(defmethod stream-location ((s lexer-stream))
  (with-slots (input-stream) s
    (stream-location input-stream)))