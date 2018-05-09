
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

(defmacro deflexer (name &key documentation whitespace terminals include-eof)
  ;; When include-eof is true, automatically add a :eof terminal
  ;; which matches end-of-file on source
  (when include-eof
    (setf terminals
          (append terminals
                  (list '(:eof ((:ignore (parse-eof))) :eof)))))
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
                        (make-lexer-for-terminal name rule))
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

;; TODO track location as the tokens' location
(defstruct (lexer-stream (:constructor make-lexer-stream-raw))
  (lexer (error "must refer to lexer") :type lexer)
  (parser (error "must supply parser") :type parser)
  (input-stream (error "need an input stream"))
  (next-result nil :type (or list parse-failure))
  (peeks nil :type list))

(defun lexer-stream (l &key (input *default-parse-input*))
  (declare (type lexer l))
  (let ((istream (new-file-stream input)))
    (make-lexer-stream-raw :lexer l
                           :parser (lexer-parser l)
                           :input-stream istream
                           :next-result
                           (multiple-value-bind (tok val loc)
                               (eval-parser (lexer-parser l) :input istream)
                             (list tok val loc)))))

;; TODO there is some problem with handling EOF
(defmethod get-stream ((s lexer-stream) (ctxt null))
  (declare (ignore ctxt))
  (with-slots (peeks input-stream parser next-result) s
    (if peeks
        ;; If we have peeked results...
        (let ((top (pop peeks)))
          (values-list top))

        ;; If not...
        (let ((res next-result))
          (when (not (listp res))
            (error res))
          ;; if EOF, no need to get the next token in stream at all,
          ;; instead just skip the call to eval-parser
          (unless (eq (cadr res) :eof)
            (handler-case
                (multiple-value-bind (tok val loc)
                    (eval-parser parser :input input-stream)
                  (setf next-result (list tok val loc)))
              (parse-failure (err)
                (setf next-result err))))
          (assert (typep res 'list))
          (values-list res)))))

(defmethod get-stream ((s lexer-stream) (ctxt parse-context))
  (multiple-value-bind (tok val loc) (get-stream s nil)
    (push-obj (list tok val loc) ctxt)
    (values tok val loc)))

(defmethod put-stream (obj (s lexer-stream))
  (push obj (lexer-stream-peeks s)))

(defmethod peek-stream ((s lexer-stream) ctxt)
  (with-slots (peeks next-result) s
    (if peeks
        (values-list (car peeks))
      (values-list next-result))))

(defmethod stream-location ((s lexer-stream))
  (with-slots (next-result input-stream) s
    (if (listp next-result)
        (third next-result)
        (stream-location input-stream))))