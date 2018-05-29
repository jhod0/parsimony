(in-package :parsimony/grammar-compiler)


(defstruct (grammar (:constructor make-grammar-struct))
  (name nil :type symbol)
  (terminals nil :type list)
  (nonterminals nil :type list)
  (lexer (error "needs lexer") :type lexer)
  (default-entry (error "default entry required")
    :type symbol))

(defun make-parser-name (grammar-name rule-name)
  (intern (concatenate 'string
            "PARSE-" (symbol-name grammar-name) "-" (symbol-name rule-name))
          (symbol-package grammar-name)))

(defun create-grammar-parser-rule (rule body parser-names)
  (let ((this-clause (when rule (car rule)))
        (rest-clauses (when rule (cdr rule))))
    (cond
     ((not rule)
      `(progn ,@body))
     ((and (keywordp this-clause)
           (assoc this-clause parser-names))
      `(with-parsed (ctxt)
                    ((:ignore (,(cdr (assoc this-clause parser-names)))))
                    ,(create-grammar-parser-rule rest-clauses body
                                                 parser-names)))

     ((keywordp this-clause)
      (let ((a (gensym)))
        `(let ((,a (next)))
           (unless (eq ,a ,this-clause)
             (fail ,a))
           ,(create-grammar-parser-rule rest-clauses body
                                        parser-names))))

     ((and (consp this-clause)
           (assoc (car this-clause) parser-names))
      `(with-parsed (ctxt)
                    ((,(cadr this-clause) (,(cdr (assoc (car this-clause) parser-names)))))
                    ,(create-grammar-parser-rule rest-clauses body
                                                 parser-names)))

     ((consp this-clause)
      (let ((a (gensym)))
        `(multiple-value-bind (,a ,(cadr this-clause)) (next)
           (unless (eq ,a ,(car this-clause))
             (fail ,a))
           ,(create-grammar-parser-rule rest-clauses body
                                        parser-names))))

     (t (error "invalid grammar rule ~a (~a ~a)" rule rest-clauses body)))))


(defun create-grammar-parser (fnname args rules parser-names)
  `(defun ,fnname ,args
     (alternative
      ,@(loop for rule in rules
              collect
              `(make-parser ',(gensym (concatenate 'string (symbol-name fnname) "-RULE")) ()
                            ,(progn
                               (when (keywordp rule)
                                 (let ((a (gensym)))
                                   (setf rule `(((,rule ,a)) ,a))))
                               (create-grammar-parser-rule (car rule) (cdr rule) parser-names)))))))

(defmacro defgrammar (grammar-name &key rules lexer extra-args default-entry description)
  (declare (ignorable description))
  (unless rules (error "no rules given to grammar"))
  (unless lexer (error "no lexer given to grammar"))
  (unless default-entry (setf default-entry (caar rules)))
  (let* ((nonterminals (mapcar #'car rules))
         (new-parser-names (mapcar #'(lambda (nt) (cons nt (make-parser-name grammar-name nt)))
                                   nonterminals)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)

       ;; verify terminals and non-terminals
       (eval-when (:compile-toplevel :execute)
         (let ((known-inputs (append (lexer-terminals ,lexer)
                                     (list ,@nonterminals))))
           (dolist (nonterminal ',rules)
             (dolist (rule (cdr nonterminal))
               (if (keywordp rule)
                   (setf rule (list rule))
                   (setf rule (car rule)))
               (dolist (input rule)
                 (let ((target (if (keywordp input)
                                   input
                                   (car input))))
                   (assert (member target known-inputs)
                           ()
                           "In expansion of lexer ~s, target ~s, rule ~s:
Target ~s is not a known terminal or nonterminal.

Available terminals: ~s
Available nonterminals: ~s"
                           ',grammar-name (car nonterminal) rule
                           target
                           (lexer-terminals ,lexer)
                           (list ,@nonterminals))))))))

       ,@(loop for nt in nonterminals
               collect (create-grammar-parser
                        (cdr (assoc nt new-parser-names)) extra-args
                        (cdr (assoc nt rules)) new-parser-names))
       (defparameter ,grammar-name
         (make-grammar-struct
          :name ',grammar-name
          :terminals (lexer-terminals ,lexer)
          :nonterminals (list
                         ,@(loop for nt in new-parser-names
                                 collect `(cons ,(car nt)
                                                #',(cdr nt))))
          :lexer ,lexer
          :default-entry ,default-entry)))))

(defun get-grammar-parser (grammar &optional target &rest extra-args)
  (with-slots (default-entry terminals
               nonterminals lexer)
      grammar
    (unless target (setf target default-entry))
    (let ((nonterm (assoc target nonterminals))
          (term (member target terminals)))
      (cond
        (nonterm (apply (cdr nonterm) extra-args))
        (term (make-parser target (((tok tokval) lexer))
                (unless (eq tok target) (fail tok))
                tokval))
        (t (error (format nil "no such target: ~a" target)))))))

(defun parse-grammar (grammar &key target (input *default-parse-input*)
                              catch-failure (raise t) (default :noparse))
  (let ((parser
         (if target
             (get-grammar-parser grammar target)
           (get-grammar-parser grammar)))
        (ls (lexer-stream (grammar-lexer grammar) :input input)))
    (eval-parser parser
                 :input ls :catch-failure catch-failure
                 :raise raise :default default)))