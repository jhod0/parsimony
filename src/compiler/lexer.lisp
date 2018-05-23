(in-package :parsimony/lexer-compiler)


(defstruct (lexer (:constructor make-lexer-struct))
  (name (error "name required") :type symbol)
  (parser (error "parser required") :type parser)
  (terminals (error "lexer needs terminals") :type list)
  (documentation nil :type (or string null)))

(defun make-lexer-name (lexer-name rule-name)
  "Constructs a name for a lexer token target, internal use."
  (intern (concatenate 'string "LEX-" (symbol-name lexer-name) "-" (symbol-name rule-name))
          *package*))

(defun make-lexer-for-terminal (lexer-name rule)
  "Constructs a function definition from a lexer rule."
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

(defun compare-char-lists (a b)
  "Comparison function for sorting lists of chars"
  (cond
    ((and (null a) (null b)) t)
    ((and (null a) b) t)
    ((null b) nil)
    ((char= (car a) (car b))
     (compare-char-lists (cdr a) (cdr b)))
    (t (char< (car a) (car b)))))

(defun literal-rule-to-node (rule)
  (let ((lit (cadr rule)))
    (list (cond
            ((and (stringp lit) (string= lit ""))
             (error "lexer literal cannot be empty string"))
            ((stringp lit)
             (coerce lit 'list))
            (t (list lit)))
          (car rule)
          (cadr rule))))

(defun compare-node (a b)
  (compare-char-lists (car a) (car b)))

(defun next-char-in-node (node)
  (when (car node)
    (caar node)))

(defun node-advance-char (node)
  (when (car node)
    (pop (car node))
    (caar node)))

(defun literal-rules-to-tree (lit-rules)
  "Converts a list of literal lexing rules into a tree format, suitable for
generating a parser.

Roughly, returns a <node-list>, where:

<node-list> := (list of <node>)

<node> := (list <character>
                [ nil | (list :keyword \"literal\") ]
                . <node-list>)"
  (let* ((char-lists (mapcar #'literal-rule-to-node lit-rules))
         (sorted-char-lists (sort char-lists #'compare-node)))
    (labels ((consume-while-letter (lst letter)
               (cond
                 ((null lst) (values nil nil))
                 ((eq letter (next-char-in-node (car lst)))
                  (node-advance-char (car lst))
                  (multiple-value-bind (let-lst rst)
                      (consume-while-letter (cdr lst) letter)
                    (values (cons (car lst)
                                  let-lst)
                            rst)))
                 (t (values nil lst))))

             (expand-letter-step (lst)
               (let ((this-node (car lst)))
                 (if (next-char-in-node this-node)
                     (cons nil (construct-tree lst))
                     (cons (cdr this-node) (construct-tree (cdr lst))))))

             (construct-tree (lst)
               (if (null lst) nil
                   (let* ((this-node (car lst))
                          (this-char (next-char-in-node this-node)))
                     (multiple-value-bind (this-letter rst)
                         (consume-while-letter lst this-char)
                       (cons (cons this-char (expand-letter-step this-letter))
                             (construct-tree rst)))))))
      (construct-tree sorted-char-lists))))

(defun literal-body-from-tree (tree loc-name)
  (labels ((gen-body-loop (default others)
             (let* ((new-char-name (gensym "new-char"))
                    (conclusion
                     (when default `(values ,@default ,loc-name)))
                    (children-action
                     (when others
                       `(case ,new-char-name
                            ,@(loop for node in others
                                 collect
                                   (list (car node)
                                         (gen-body-loop
                                             (cadr node)
                                             (cddr node))))
                          (otherwise (fail ,new-char-name))))))

               (if (and conclusion (not children-action))
                   conclusion
                   `(let ((,new-char-name (next)))
                      ,(cond
                        ((and (not conclusion)
                              (not children-action))
                         (error "???"))
                        ((not conclusion)
                         children-action)
                        (t
                         `(handler-case
                              ,children-action
                            (parse-failure ()
                              ,conclusion)))))))))
    (gen-body-loop nil tree)))

(defun make-literal-lexer-body (literals loc-name)
  (literal-body-from-tree (literal-rules-to-tree literals)
                          loc-name))

(defun make-literal-lexer (name literals literal-parser-name)
  "Generates the definition for a parser, based on a set of lexer tokens defined
as literals."
  (declare (ignorable name literals))
  (let* ((loc-name (gensym "start-loc"))
         (parser-body (make-literal-lexer-body literals loc-name)))
    `(,literal-parser-name ()
                           (make-parser ',literal-parser-name ((,loc-name :location))
                                        ,parser-body))))

(defun make-lexer-terminal-definitions (name literals rules)
  "Generates the functions, inside (labels), for use in a lexer definition."
  (let ((full-rule-definitions
         (mapcar #'(lambda (rule)
                     (make-lexer-for-terminal name rule))
                 rules)))
    (if (null literals)
        full-rule-definitions
        (cons (make-literal-lexer name literals
                                  (make-lexer-name name :literals))
              full-rule-definitions))))

(defmacro deflexer (name &key documentation whitespace terminals include-eof)
  "Generates a lexer definition, with a given name and the tokens specified in
terminals."
  ;; When include-eof is true, automatically add a :eof terminal
  ;; which matches end-of-file on source
  (when include-eof
    (setf terminals
          (append terminals
                  (list '(:eof ((:ignore (parse-eof))) :eof)))))
  ;; First split literals and full rules
  (multiple-value-bind (literal-rules full-rules)
      (partition-lexer-rules terminals)
    ;; Name all of the lexers, and generate the core parser expression
    (let* ((terminal-names (loop for n in full-rules
                                collect (the keyword (car n))))
           (literal-names (loop for n in literal-rules
                               collect (the keyword (car n))))
           (lexer-names (loop for term in terminal-names
                             collect (make-lexer-name name term)))
           (literal-lexer-name (make-lexer-name name :literals))
           (parser-core `(alternative
                          ,@(when literal-rules
                                  (list `(,literal-lexer-name)))
                          ,@(mapcar #'(lambda (name) `(,name))
                                    lexer-names)))
           (parser-core-name (gensym "parser-core"))
           (whitespace-parser-name (gensym "whitespace")))

      ;; Form to return
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (labels
             ,(make-lexer-terminal-definitions
               name literal-rules full-rules)
           (let ((,whitespace-parser-name
                  ,(when whitespace
                         `(make-parser ',(make-lexer-name name :whitespace)
                                       ((:ignore (parse-many ,whitespace))))))
                 (,parser-core-name ,parser-core))
             (declare (ignorable ,whitespace-parser-name))
             (defparameter ,name
               (make-lexer-struct :name ',name
                                  :documentation ,documentation
                                  :terminals (list ,@(append literal-names terminal-names))
                                  :parser
                                  (make-parser ',name ((:ignore ,whitespace-parser-name))
                                               (eval-in-context ,parser-core-name))))))))))

(defmacro lex (name &rest args)
  `(eval-parser (lexer-parser ,name) ,@args))

(defun get-lexer-parser (lexer)
  (lexer-parser lexer))

(defstruct (lexer-stream (:constructor make-lexer-stream-raw))
  (lexer (error "must refer to lexer") :type lexer)
  (parser (error "must supply parser") :type parser)
  (input-stream (error "need an input stream"))
  (next-result nil :type (or list parse-failure))
  (peeks nil :type list))

(defun lexer-stream (l &key (input *default-parse-input*))
  "Constructs a new lexer-stream from a given lexer, pulling from `input`.
The methods `get-stream`, `put-stream`, `peek-stream`, and `stream-location`
on the lexer-stream returned from this function."
  (declare (type lexer l))
  (let ((istream (new-file-stream input)))
    (make-lexer-stream-raw :lexer l
                           :parser (lexer-parser l)
                           :input-stream istream
                           :next-result
                           (multiple-value-bind (tok val loc)
                               (eval-parser (lexer-parser l) :input istream)
                             (list tok val loc)))))

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
    (push-obj-to-context (list tok val loc) ctxt)
    (values tok val loc)))

(defmethod put-stream (obj (s lexer-stream))
  (push obj (lexer-stream-peeks s)))

(defmethod peek-stream ((s lexer-stream) ctxt)
  (with-slots (peeks next-result) s
    (if peeks
        (values-list (car peeks))
      (values-list next-result))))

(defmethod stream-location ((s lexer-stream))
  (with-slots (peeks next-result input-stream) s
    (cond
      (peeks (copy-file-loc (third (car peeks))))
      ((listp next-result) (copy-file-loc (third next-result)))
      (t (stream-location input-stream)))))