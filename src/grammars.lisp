
(in-package :parsimony)


(defstruct (grammar (:constructor make-grammar-struct))
  (name nil :type symbol)
  (terminals nil :type list)
  (nonterminals nil :type list)
  (default-entry (error "default entry required")
    :type symbol))


(defun make-parser-name (grammar-name rule-name)
  (intern (concatenate "PARSE-" grammar-name "-" (symbol-name rule-name))
          *package*))

(defmacro defgrammar (grammar-name &rest args)
  (let* ((terminals-arg (cadr (assoc :terminals args)))
         (default-entry (cadr (assoc :default-entry args)))
         (rules-arg (cadr (assoc :rules args)))

         (terminal-names (mapcar #'car terminals-arg))
         (known-parsers (append (mapcar #'(lambda (a) (cons (car a) (cadr a)))
                                        terminals-arg)
                                (mapcar #'(lambda (rule)
                                            (cons (car rule) (make-parser-name grammar-name (car rule))))
                                        rules-arg))))
    `(progn
       ,@(mapcar #'(lambda (rule) (declare (ignore rule)) (error "unimplemented"))
                 rules-arg)
       
       (defconstant ,grammar-name (make-grammar-struct :name ,grammar-name
                :terminals (list ,@(mapcar #'(lambda (n) `(cons ,(car n) #',(cadr n)))
                                           terminals-arg))
                :default-entry ,default-entry)))))
