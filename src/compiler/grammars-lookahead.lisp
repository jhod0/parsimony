(in-package :parsimony/grammar-compiler)

;; TODO
;; Generates a lookahead machine, to scan the coming input and
;; determine the grammar rules to be used.

(defstruct grammar-rule
  (nonterminal nil :type keyword)
  (rule-number 0 :type fixnum)
  (inputs nil :type list)
  (arguments nil :type list)
  (body nil :type list))

(defstruct decision-tree
  (nonterminal nil :type keyword)
  (transitions nil :type list)
  (finish nil :type (or grammar-rule null)))

(defstruct grammar-compile-time-definition
  (name nil :type symbol)
  (terminals nil :type list)
  (nonterminals nil :type list)
  (default-entry (error "needs a default") :type symbol)
  (rules nil :type list)
  (trees nil :type list))

(define-condition grammar-compile-failure (error)
  ((grammar-name :accessor gc-failure-grammar
                 :type symbol
                 :initarg :name
                 :initform (error "failure happened on A grammar"))))


;; ===================
;; Basic Grammar Rules
;; ===================
;; GRAMMAR-RULE structs will be used by the rest of the grammar analysis system,
;; and so we need to convert the raw form given to DEFGRAMMAR into GRAMMAR-RULE
;; structures. The following two functions will accomplish that.

(defun internalize-rule (nonterminal number body)
  "Creates a GRAMMAR-RULE struct for a rule targeting NONTERMINAL, with rule number
NUMBER, and following the parse path & side effects specified by BODY."
  (labels ((partition-inputs-args (definition)
             (cond
               ((keywordp definition)
                (values (list definition) (list nil)))
               ((null definition)
                (values nil nil))
               (t
                (multiple-value-bind (inputs args)
                    (partition-inputs-args (cdr definition))
                  (if (keywordp (car definition))
                      (values (cons (car definition) inputs)
                              (cons nil args))
                      (values (cons (caar definition) inputs)
                              (cons (cadar definition) args))))))))
    (multiple-value-bind (inputs args)
        (if (keywordp body)
            (values (list body) (list nil))
            (partition-inputs-args (car body)))
      (make-grammar-rule :nonterminal nonterminal
                         :rule-number number
                         :inputs inputs
                         :arguments args
                         :body (when (consp body)
                                 (cons 'progn (cdr body)))))))

(defun intern-all-rules (rules)
  "Convert a list of raw grammar rules into GRAMMAR-RULE structs, returning an
association list of association lists, mapping
<nonterminal target> => <rule number> => <GRAMMAR-RULE>"
  (let ((rule-n 0)
        output-rules)
    (declare (dynamic-extent output-rules)
             (type fixnum rule-n))
    (dolist (nt rules)
      (let ((target (car nt))
            (these-rules (cdr nt))
            (these-output-rules))
        (declare (dynamic-extent these-output-rules))
        (dolist (rule these-rules)
          (push (cons (incf rule-n)
                      (internalize-rule
                       target rule-n rule))
                these-output-rules))
        (push (cons target (nreverse these-output-rules))
              output-rules)))
    (nreverse output-rules)))


;; ==============
;; Decision Trees
;; ==============
;; Tools for creating decision trees from the rules created above.
;; A decision tree is a simple representation of the state machine for determining
;; whether input fits a grammar, and what rules to invoke on the input.

(defun interned-rules-to-trees (rules)
  "Creates a decision tree for each nonterminal target, given the sorted rules
from INTERN-ALL-RULES. Returns an association list mapping nonterminal
keywords to DECISION-TREE structs."
  (labels ((make-decision-tree (target rules)
             (error "unimplemented")))
    (loop for group in rules
       collect (cons (car group)
                     (make-decision-tree
                      (car group)
                      (cdr group))))))


;; ===========================
;; Compile Time Representation
;; ===========================
;; Bringing all the above together, converts a user-input grammar into an
;; internal representation to be used at compile-time. From this, the code for
;; both the lookahead machine and the parser executer will be generated.

(defun make-compile-time-grammar (name terminals nonterminals default-entry rules)
  "Creates a GRAMMAR-COMPILE-TIME-DEFINITION for internal compiler use."
  (let ()
    (make-grammar-compile-time-definition
     :name name
     :terminals terminals
     :nonterminals nonterminals
     :default-entry default-entry
     :rules (error "unimplemented")
     :trees (error "unimplemented"))))