; apply-in-underlying-scheme procedure (§4.1.4 Footnote 17)
(define apply-in-underlying-scheme apply)

#|
 | §4.1.1 'The Core of the Evaluator'
 |#

; eval procedure
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((and? exp) (eval-and exp env))         ;!
        ((or? exp) (eval-or exp env))           ;!
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

; apply procedure
(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))

; list-of-values procedure
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

; eval-if procedure
(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

; eval-sequence procedure
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

; eval-assignment procedure
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

; eval-definition procedure
(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)

#|
 | §4.1.2 'Representing Expressions'
 |#

; self-evaluating? predicate procedure
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

; variable? predicate procedure
(define (variable? exp) (symbol? exp))

; quoted? predicate procedure
(define (quoted? exp)
  (tagged-list? exp 'quote))

; text-of-quotation selector procedure
(define (text-of-quotation exp) (cadr exp))

; tagged-list? predicate procedure
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

; assignment? predicate procedure
(define (assignment? exp)
  (tagged-list? exp 'set!))

; assignment-variable selector procedure
(define (assignment-variable exp) (cadr exp))

; assignment-value selector procedure
(define (assignment-value exp) (caddr exp))

; definition predicate procedure
(define (definition? exp)
  (tagged-list? exp 'define))

; definition-variable selector procedure
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

; definition-value selector procedure
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)   ; formal parameters
                   (cddr exp)))) ; body

; lambda? predicate procedure
(define (lambda? exp) (tagged-list? exp 'lambda))

; lambda-parameters selector procedure
(define (lambda-parameters exp) (cadr exp))

; lambda-body selector procedure
(define (lambda-body exp) (cddr exp))

; make-lambda constructor procedure
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

; if? predicate procedure
(define (if? exp) (tagged-list? exp 'if))

; if-predicate selector procedure
(define (if-predicate exp) (cadr exp))

; if-consequent selector procedure
(define (if-consequent exp) (caddr exp))

; if-alternative selector procedure
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

; make-if constructor procedure
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

; begin? predicate procedure
(define (begin? exp) (tagged-list? exp 'begin))

; begin-actions selector procedure
(define (begin-actions exp) (cdr exp))

; last-exp? predicate procedure
(define (last-exp? seq) (null? (cdr seq)))

; first-exp selector procedure
(define (first-exp seq) (car seq))

; rest-exps selector procedure
(define (rest-exps seq) (cdr seq))

; sequence->exp procedure
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

; make-begin constructor procedure
(define (make-begin seq) (cons 'begin seq))

; application? predicate procedure
(define (application? exp) (pair? exp))

; operator selector procedure
(define (operator exp) (car exp))

; operands selector procedure
(define (operands exp) (cdr exp))

; no-operands? predicate procedure
(define (no-operands? ops) (null? ops))

; first-operand selector procedure
(define (first-operand ops) (car ops))

; rest-operands selector procedure
(define (rest-operands ops) (cdr ops))

; cond? predicate procedure
(define (cond? exp) (tagged-list? exp 'cond))

; cond-clauses selector procedure
(define (cond-clauses exp) (cdr exp))

; cond-else-clause? predicate procedure
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

; cond-predicate selector procedure
(define (cond-predicate clause) (car clause))

; cond-actions selector procedure
(define (cond-actions clause) (cdr clause))

; cond->if procedure
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

; expand-clauses procedure
(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

#|
 | §4.1.3 'Evaluator Data Structures'
 |#

; true? predicate procedure
(define (true? x)
  (not (eq? x false)))

; false? predicate procedure
(define (false? x)
  (eq? x false))

; make-procedure constructor procedure
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

; compound-procedure? predicate procedure
(define (compound-procedure? p)
  (tagged-list? p 'procedure))

; procedure-parameters selector procedure
(define (procedure-parameters p) (cadr p))

; procedure-body selector procedure
(define (procedure-body p) (caddr p))

; procedure-environment selector procedure
(define (procedure-environment p) (cadddr p))

; enclosing-environment selector procedure
(define (enclosing-environment env) (cdr env))

; first-frame selector procedure
(define (first-frame env) (car env))

; the-empty-environment def'n
(define the-empty-environment '())

; make-frame constructor procedure
(define (make-frame variables values)
  (cons variables values))

; frame-variables selector procedure
(define (frame-variables frame) (car frame))

; frame-values selector procedure
(define (frame-values frame) (cdr frame))

; add-binding-to-frame! mutator procedure
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

; extend-enviroment constructor procedure
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

; lookup-variable-value procedure
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

; set-variable-value! mutator procedure
(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

; define-variable! mutator procedure
(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

#|
 | §4.1.4 'Running the Evaluator as a Program'
 |#

; setup-environment side-effect procedure
(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

; primitive-procedure? predicate procedure
(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

; primitive-implementation selector procedure
(define (primitive-implementation proc) (cadr proc))

; primitive-procedures list def'n
(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
;;      more primitives
        ))

; primitive-procedure-names selector procedure
(define (primitive-procedure-names)
  (map car
       primitive-procedures))

; primitive-procedure-objects selector procedure
(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

; apply-primitive-procedure procedure
(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

; input-prompt def'n
(define input-prompt ";;; M-Eval input:")

; output-prompt def'n
(define output-prompt ";;; M-Eval value:")

; driver-loop procedure (REPL)
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

; prompt-for-input procedure
(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

; announce-output procedure
(define (announce-output string)
  (newline) (display string) (newline))

; user-print procedure
(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

; the-global-environment def'n
; (define the-global-environment (setup-environment))

; start REPL
; (driver-loop)

#|
 | Exercise 4.4
 |
 | Recall the definitions of the special forms and and or from chapter 1:
 |
 | • 'and': The expressions are evaluated from left to right. If any expression
 | evaluates to false, false is returned; any remaining expressions are not
 | evaluated. If all the expressions evaluate to true values, the value of the
 | last expression is returned. If there are no expressions then true is
 | returned.
 |
 | • 'or': The expressions are evaluated from left to right. If any expression
 | evaluates to a true value, that value is returned; any remaining expressions
 | are not evaluated. If all expressions evaluate to false, or if there are no
 | expressions, then false is returned.
 |
 | Install 'and' and 'or' as new special forms for the evaluator by defining
 | appropriate syntax procedures and evaluation procedures 'eval-and' and
 | 'eval-or'. Alternatively, show how to implement 'and' and 'or' as derived
 | expressions.
 |
 | Answer
 |
 | [| Instructor's Manual |]
 | §4.1.2 Representing Expressions -> Derived expressions
 |
 |    In exercise 4.4, implementing 'and' and 'or' as derived expressions has
 |    some subtleties. The most straightforward transformation has the flaw
 |    that it evaluates subexpressions more than once (to test the truth of a
 |    subexpression and then return its value). Students can be expected to
 |    notice this and avoid this duplicate evaluation by introducing 'let'
 |    into the transformed expression. But this will introduce a variable
 |    name, which might capture a free variable in the user's expression. We
 |    have not given students tools to avoid this by generating unique
 |    variable names, and would not expect them to note the name-capture
 |    problem in their solution.
 |
 |    A very clever transformation is given in the Scheme standard (section
 |    7.6) which uses 'let' to avoid duplicate evaluation and yet cannot
 |    capture the user's free-variables: '(or <e1> <e2> ... <en>)' is
 |    transformed to
 |
 |    (let ((first-exp <e1>)
 |          (rest-of-or (lambda () (or <e2> ... <en>))))
 |      (if first-exp first-exp (rest-of-or)))
 |
 |    and '(and <e1> <e2> ... <en>)' is transformed to
 |
 |    (let ((first-exp <e1>)
 |          (rest-of-and (lambda () (and <e2> ... <en>))))
 |      (if first-exp (rest-of-and) first-exp))
 |
 |
 | TODO: Implement 'and' and 'or' as derived expressions after implementing
 | 'let' to avoid the name-capture problem
 |#

; and? predicate procedure
(define (and? exp) (tagged-list? exp 'and))

; or? predicate procedure
(define (or? exp) (tagged-list? exp 'or))

; logic-preds selector procedure
(define (logic-preds exp) (cdr exp))

; no-preds? predicate procedure
(define (no-preds? pred-seq) (null? pred-seq))

; last-pred? predicate procedure
(define (last-pred? pred-seq) (null? (cdr pred-seq)))

; first-pred selector procedure
(define (first-pred pred-seq) (car pred-seq))

; rest-preds selector procedure
(define (rest-preds pred-seq) (cdr pred-seq))

; eval-and procedure (direct impl)
(define (eval-and exp env)
  (define (rec pred-seq)
    (let ((val (eval (first-pred pred-seq) env)))
      (cond ((last-pred? pred-seq) val)
            ((false? val) val)
            (else (rec (rest-preds pred-seq))))))
  (let ((pred-seq (logic-preds exp)))
    (if (no-preds? pred-seq)
        true
        (rec pred-seq))))

; eval-or procedure (direct impl)
(define (eval-or exp env)
  (define (rec pred-seq)
    (let ((val (eval (first-pred pred-seq) env)))
      (cond ((last-pred? pred-seq) val)
            ((true? val) val)
            (else (rec (rest-preds pred-seq))))))
  (let ((pred-seq (logic-preds exp)))
    (if (no-preds? pred-seq)
        false
        (rec pred-seq))))

(define E0 (setup-environment))

(eval-and '(and) E0)
(eval-and '(and false) E0)
(eval-and '(and true) E0)
(eval-and '(and 0) E0)
(eval-and '(and '()) E0)
(eval-and '(and false '()) E0)
(eval-and '(and '() false) E0)
(eval-and '(and 0 '()) E0)
(eval-and '(and '() 0) E0)
(eval-and '(and false false) E0)

(eval-or '(or) E0)
(eval-or '(or false) E0)
(eval-or '(or true) E0)
(eval-or '(or 0) E0)
(eval-or '(or '()) E0)
(eval-or '(or false '()) E0)
(eval-or '(or '() false) E0)
(eval-or '(or 0 '()) E0)
(eval-or '(or '() 0) E0)
(eval-or '(or false false) E0)

; eval-and procedure (derived expression)
(define (eval-and exp env)
  (eval (and->if exp) env))

; and->if procedure (convert 'and' expr to nested 'if' expr)
(define (and->if exp)
  (expand-and (logic-preds exp)))

; expand-and procedure (recursively construct nested 'if' expr)
; TODO: Implement 'and' and 'or' as derived expressions after implementing
; 'let' to avoid the name-capture problem
(define (expand-and pred-seq)
  (if (null? pred-seq)
      'true
      (let ((first (first-pred pred-seq))
            (rest (rest-preds pred-seq)))
        (if (null? rest)
            first
            (make-if first (expand-and rest) 'false)))))

(eval-and '(and) E0)
(eval-and '(and false) E0)
(eval-and '(and true) E0)
(eval-and '(and 0) E0)
(eval-and '(and '()) E0)
(eval-and '(and false '()) E0)
(eval-and '(and '() false) E0)
(eval-and '(and 0 '()) E0)
(eval-and '(and '() 0) E0)
(eval-and '(and false false) E0)

; eval-or procedure (derived expression)
(define (eval-or exp env)
  (eval (or->if exp) env))

; or->if procedure (convert 'or' expr to nested 'if' expr)
(define (or->if exp)
  (expand-or (logic-preds exp)))

; expand-or procedure (recursively construct nested 'if' expr)
; TODO: Implement 'and' and 'or' as derived expressions after implementing
; 'let' to avoid the name-capture problem
(define (expand-or pred-seq)
  (if (null? pred-seq)
      'false
      (let ((first (first-pred pred-seq))
            (rest (rest-preds pred-seq)))
        (if (null? rest)
            first
            (cons (make-lambda '(x) (list (make-if 'x 'x (expand-or rest))))
                  (list first))))))

(eval-or '(or) E0)
(eval-or '(or false) E0)
(eval-or '(or true) E0)
(eval-or '(or 0) E0)
(eval-or '(or '()) E0)
(eval-or '(or false '()) E0)
(eval-or '(or '() false) E0)
(eval-or '(or 0 '()) E0)
(eval-or '(or '() 0) E0)
(eval-or '(or false false) E0)

(define exp1 (and->if '(and e1 e2 e3)))
(begin (newline) (user-print exp1))

(define exp2 (or->if '(or e1 e2 e3)))
(begin (newline) (user-print exp2))
