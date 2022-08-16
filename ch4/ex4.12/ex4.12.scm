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
        ((let? exp) (eval (let->combination exp) env))
        ((let*? exp) (eval (let*->nested-lets exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((and? exp) (eval (and->let exp) env))
        ((or? exp) (eval (or->let exp) env))
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
(define (quoted? exp) (tagged-list? exp 'quote))

; text-of-quotation selector procedure
(define (text-of-quotation exp) (cadr exp))

; tagged-list? predicate procedure
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

; assignment? predicate procedure
(define (assignment? exp) (tagged-list? exp 'set!))

; assignment-variable selector procedure
(define (assignment-variable exp) (cadr exp))

; assignment-value selector procedure
(define (assignment-value exp) (caddr exp))

; definition predicate procedure
(define (definition? exp) (tagged-list? exp 'define))

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

; make-definition constructor procedure
(define (make-definition variable value) (cons 'define (cons variable value)))

; lambda? predicate procedure
(define (lambda? exp) (tagged-list? exp 'lambda))

; lambda-parameters selector procedure
(define (lambda-parameters exp) (cadr exp))

; lambda-body selector procedure
(define (lambda-body exp) (cddr exp))

; make-lambda constructor procedure
(define (make-lambda parameters body) (cons 'lambda (cons parameters body)))

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

; let? predicate procedure
(define (let? exp) (tagged-list? exp 'let))

; named-let? predicate procedure
(define (named-let? exp)
  (symbol? (cadr exp)))

; let-bindings selector procedure
(define (let-bindings exp)
  (if (named-let? exp)
      (caddr exp)
      (cadr exp)))

; let-body selector procedure
(define (let-body exp)
  (if (named-let? exp)
      (cadddr exp)
      (caddr exp)))

; zip procedure
(define (zip xs ys)
  (cond ((and (null? xs) (null? ys))
         '())
        ((or (null? xs) (null? ys))
         (error "Unequal lengths -- ZIP" (list xs ys)))
        (else
         (cons (list (car xs) (car ys))
               (zip (cdr xs) (cdr ys))))))

; unzip procedure
(define (unzip zs)
  (if (null? zs)
      (list '() '())
      (let ((res (unzip (cdr zs))))
        (let ((xs (car res))
              (ys (cadr res)))
          (list (cons (caar zs) xs)
                (cons (cadar zs) ys))))))

; let->combination procedure
(define (let->combination exp)
  (let ((bindings (let-bindings exp))
        (body (let-body exp)))
    (let ((res (unzip bindings)))
      (if (named-let? exp)
          (make-begin
           (list (make-definition
                  (cons (cadr exp) (car res))
                  (list body))
                 (cons (cadr exp) (cadr res))))
          (cons (make-lambda
                 (car res)
                 (list body))
                (cadr res))))))

; make-let constructor procedure
(define (make-let bindings body) (cons 'let (cons bindings body)))

; let*? predicate procedure
(define (let*? exp) (tagged-list? exp 'let*))

; let*->nested-lets procedure
(define (let*->nested-lets exp) (expand-let* (let-bindings exp) (let-body exp)))

; expand-let* proceudre
(define (expand-let* bindings body)
  (if (null? bindings)
      body
      (make-let (list (car bindings))
                (list (expand-let* (cdr bindings) body)))))

; cond? predicate procedure
(define (cond? exp) (tagged-list? exp 'cond))

; cond-clauses selector procedure
(define (cond-clauses exp) (cdr exp))

; cond-else-clause? predicate procedure
(define (cond-else-clause? clause) (eq? (cond-predicate clause) 'else))

; cond-predicate selector procedure
(define (cond-predicate clause) (car clause))

; cond-actions selector procedure
(define (cond-actions clause) (cdr clause))

; cond-arrow? predicate procedure
(define (cond-arrow? clause) (eq? (cadr clause) '=>))

; cond-test selector procedure
(define (cond-test clause) (car clause))

; cond-recipient selector procedure
(define (cond-recipient clause) (caddr clause))

; cond->if procedure
(define (cond->if exp) (expand-clauses (cond-clauses exp)))

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
            (if (cond-arrow? first)
                (make-let
                 (zip '(test recipient)
                      (list (cond-test first) (cond-recipient first)))
                 (list (make-if 'test
                                '(recipient test)
                                (expand-clauses rest))))
                (make-if (cond-predicate first)
                         (sequence->exp (cond-actions first))
                         (expand-clauses rest)))))))

#|
 | §4.1.3 'Evaluator Data Structures'
 |#

; true? predicate procedure
(define (true? x) (not (eq? x false)))

; false? predicate procedure
(define (false? x) (eq? x false))

; make-procedure constructor procedure
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

; compound-procedure? predicate procedure
(define (compound-procedure? p) (tagged-list? p 'procedure))

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
(define (make-frame variables values) (cons variables values))

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

; frame-lookup-cps procedure
(define (frame-lookup-cps var vars vals sc fc)    ;!
  (cond ((null? vars) (fc))
        ((eq? var (car vars)) (sc vals))
        (else (frame-lookup-cps var (cdr vars) (cdr vals) sc fc))))

; env-lookup-cps procedure
(define (env-lookup-cps var env sc fc)            ;!
  (if (eq? env the-empty-environment)
      (fc)
      (let ((frame (first-frame env)))
        (let ((vars (frame-variables frame))
              (vals (frame-values frame))
              (enc (enclosing-environment env)))
          (let ((fc (lambda () (env-lookup-cps var enc sc fc))))
            (frame-lookup-cps var vars vals sc fc))))))

; lookup-variable-value procedure
(define (lookup-variable-value var env)           ;!
  (env-lookup-cps
   var
   env
   (lambda (vals) (car vals))
   (lambda () (error "Unbound variable" var))))

; set-variable-value! mutator procedure
(define (set-variable-value! var val env)         ;!
  (env-lookup-cps
   var
   env
   (lambda (vals) (set-car! vals val))
   (lambda () (error "Unbound variable -- SET!" var))))


; define-variable! mutator procedure
(define (define-variable! var val env)            ;!
  (let ((frame (first-frame env)))
    (let ((vars (frame-variables frame))
          (vals (frame-values frame)))
      (frame-lookup-cps
        var
        vars
        vals
        (lambda (vals) (set-car! vals val))
        (lambda () (add-binding-to-frame! var val frame))))))

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
(define (primitive-procedure? proc) (tagged-list? proc 'primitive))

; primitive-implementation selector procedure
(define (primitive-implementation proc) (cadr proc))

; primitive-procedures list def'n
(define primitive-procedures
  (list (list '* *)
        (list '+ +)
        (list '- -)
        (list '/ /)
        (list '= =)
        (list 'assoc assoc)
        (list 'car car)
        (list 'cadr cadr)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
;;      more primitives
        ))

; primitive-procedure-names selector procedure
(define (primitive-procedure-names) (map car primitive-procedures))

; primitive-procedure-objects selector procedure
(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc))) primitive-procedures))

; apply-primitive-procedure procedure
(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme (primitive-implementation proc) args))

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

; make-and constructor procedure
(define (make-and pred-seq) (cons 'and pred-seq))

; make-or constructor procedure
(define (make-or pred-seq) (cons 'or pred-seq))

; and->let procedure
(define (and->let exp)
  (let ((pred-seq (logic-preds exp)))
    (if (null? pred-seq)
        'true
        (let ((first (first-pred pred-seq))
              (rest (rest-preds pred-seq)))
          (if (null? rest)
              first
              (make-let
               (zip '(first-exp rest-of-and)
                    (list first (make-lambda '() (list (make-and rest)))))
               (list (make-if 'first-exp '(rest-of-and) 'first-exp))))))))

; or->let procedure
(define (or->let exp)
  (let ((pred-seq (logic-preds exp)))
    (if (null? pred-seq)
        'false
        (let ((first (first-pred pred-seq))
              (rest (rest-preds pred-seq)))
          (if (null? rest)
              first
              (make-let
               (zip '(first-exp rest-of-or)
                    (list first (make-lambda '() (list (make-or rest)))))
               (list (make-if 'first-exp 'first-exp '(rest-of-or)))))))))

#|
 | Exercise 4.12
 |
 | The procedures 'set-variable-value!', 'define-variable!', and
 | 'lookup-variable-value' can be expressed in terms of more abstract
 | procedures for traversing the environment structure. Define abstractions
 | that capture the common patterns and redefine the three procedures in
 | terms of these abstractions.
 |#

(define E0 (setup-environment))
(eval '(define foo 0) E0)
(eval 'foo E0)
(eval 'bar E0)
(restart 1)
(eval '(set! foo 1) E0)
(eval 'foo E0)
(eval '(set! bar 2) E0)
(restart 1)
(eval '(define foo 3) E0)
(eval 'foo E0)
(eval '(define bar 4) E0)
(eval 'bar E0)
(define exp '(let* ((x 3) (y (+ x 2)) (z (+ x y 5))) (+ (* x z) foo)))
(eval exp E0)
E0
