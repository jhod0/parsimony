
(in-package :parsimony)


(defstruct (grammar (:constructor make-grammar-struct))
  (name nil :type symbol)
  (terminals nil :type list)
  (nonterminals nil :type list)
  (lexer (error "needs lexer") :type lexer)
  (default-entry (error "default entry required")
    :type symbol))


(defun make-parser-name (grammar-name rule-name)
  (intern (concatenate "PARSE-" grammar-name "-" (symbol-name rule-name))
          *package*))

(defmacro defgrammar (grammar-name &rest args)
  (labels ((get-arg-optional (name &optional default)
             (cdr (or (assoc name args)
                       (list default))))
           (get-arg (name)
             (let ((a (get-arg-optional name)))
               (unless a (error (format nil "missing argument to defgrammar: ~a" name)))
               a)))
    (let* ((rules-arg (get-arg :rules))
           (nonterminals (mapcar #'car rules-arg))
           (lexer-arg (car (get-arg :lexer)))
           (description (get-arg-optional :description)))
      (declare (ignorable description))
      `(progn
         (let ((known-inputs (append (lexer-terminals ,lexer-arg)
                                     (list ,@nonterminals))))
           (dolist (nonterminal ',(mapcar #'cdr rules-arg))
             (dolist (rule (mapcar #'car nonterminal))
               (dolist (input rule)
                 (if (keywordp input)
                     (assert (member input known-inputs))
                   (assert (member (car input) known-inputs)))))))
         ',grammar-name))))

#|
         ,@(mapcar #'(lambda (rule) (declare (ignore rule)) (error "unimplemented"))
                   rules-arg)
       
       (defconstant ,grammar-name (make-grammar-struct :name ,grammar-name
                :terminals (list ,@(mapcar #'(lambda (n) `(cons ,(car n) #',(cadr n)))
                                           terminals-arg))
                :default-entry ,default-entry)))))

|#
