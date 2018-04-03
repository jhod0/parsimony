
(in-package :parsimony)


(defstruct (lexer (:constructor make-lexer-struct))
  (name (error "name required") :type symbol)
  (parser (error "parser required") :type parser)
  (documentation nil :type (or string null)))

(defun make-lexer-name (lexer-name rule-name)
  (intern (concatenate 'string "LEX-" (symbol-name lexer-name) "-" (symbol-name rule-name))
          *package*))

(defmacro deflexer (name &key documentation terminals)
  (let* ((terminal-names (mapcar #'(lambda (n) (the keyword (car n)))
                                 terminals))
         (lexer-names (mapcar #'(lambda (term) (make-lexer-name name term))
                              terminal-names)))
    `(progn
       ,@(mapcar #'(lambda (rule)
                     `(defparser ,(make-lexer-name name (car rule)) ()
                        ,@(if (cddr rule)
                              `(,(cadr rule)
                                (values ,(car rule) (progn ,@(cddr rule))))
                            (let ((tmp (gensym)))
                              `(((,tmp ,(cadr rule)))
                                (values ,(car rule) ,tmp))))))
                 terminals)
       (defconstant ,name
         (make-lexer-struct :name ',name
           :documentation ,documentation
           :parser (alternative ,@(mapcar #'(lambda (name)
                                              `(,name))
                                          lexer-names)))))))

(defmacro lex (name &rest args)
  `(eval-parser (lexer-parser ,name) ,@args))


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

(defmethod get-stream ((s lexer-stream))
  (if (lexer-stream-peeks s)
      (pop (lexer-stream-peeks s))
    (eval-parser (lexer-stream-parser s)
                 :input (lexer-stream-input-stream s))))

(defmethod put-stream (obj (s lexer-stream))
  (push obj (lexer-stream-peeks s)))

(defmethod peek-stream ((s lexer-stream))
  (if (lexer-stream-peeks s)
      (car (lexer-stream-peeks s))
    (progn
      (push (get-stream s) (lexer-stream-peeks s))
      (peek-stream s))))
