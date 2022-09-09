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
      (cdddr exp)
      (cddr exp)))

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
                  body)
                 (cons (cadr exp) (cadr res))))
          (cons (make-lambda
                 (car res)
                 body)
                (cadr res))))))

; make-let constructor procedure
(define (make-let bindings body) (cons 'let (cons bindings body)))

; let*? predicate procedure
(define (let*? exp) (tagged-list? exp 'let*))

; let*->nested-lets procedure
(define (let*->nested-lets exp) (expand-let* (let-bindings exp) (let-body exp)))

; expand-let* procedure
(define (expand-let* bindings body)
  (cond ((null? bindings)
         (make-let bindings body))
        ((null? (cdr bindings))
         (make-let (list (car bindings)) body))
        (else
         (make-let (list (car bindings))
                   (list (expand-let* (cdr bindings) body))))))

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

; unassigned? predicate procedure
(define (unassigned? x)
  (eq? x '*unassigned*))

; scan-out-defines procedure
(define (scan-out-defines body)
  (let ((res (partition-defns body)))
    (let ((defns (car res))
          (exprs (cdr res)))
      (if (null? defns)                           ; prevents infinite loop!
          exprs
          (let ((binds (bind-defns defns))
                (assigns (assign-defns defns)))
            (list (make-let binds (append assigns exprs))))))))

; partition-defns procedure
(define (partition-defns body)
  (if (null? body)
      (cons '() '())
      (let ((hd (car body))
            (res (partition-defns (cdr body))))
        (let ((defns (car res))
              (exprs (cdr res)))
          (if (definition? hd)
              (cons (cons hd defns) exprs)
              (cons defns (cons hd exprs)))))))

; bind-defns procedure
(define (bind-defns defns)
  (if (null? defns)
      '()
      (cons (list (definition-variable (car defns))
                  ''*unassigned*)                 ; note double-quote
            (bind-defns (cdr defns)))))

; assign-defns procedure
(define (assign-defns defns)
  (if (null? defns)
      '()
      (cons (list 'set!
                  (definition-variable (car defns))
                  (definition-value (car defns)))
            (assign-defns (cdr defns)))))

; make-procedure constructor procedure
(define (make-procedure parameters body env)
  (list 'procedure parameters (scan-out-defines body) env))

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
(define (frame-lookup-cps var vars vals sc fc)
  (cond ((null? vars) (fc))
        ((eq? var (car vars)) (sc vals))
        (else (frame-lookup-cps var (cdr vars) (cdr vals) sc fc))))

; env-lookup-cps procedure
(define (env-lookup-cps var env sc fc)
  (if (eq? env the-empty-environment)
      (fc)
      (let ((frame (first-frame env)))
        (let ((vars (frame-variables frame))
              (vals (frame-values frame))
              (enc (enclosing-environment env)))
          (let ((fc (lambda () (env-lookup-cps var enc sc fc))))
            (frame-lookup-cps var vars vals sc fc))))))

; lookup-variable-value procedure
(define (lookup-variable-value var env)
  (env-lookup-cps
   var
   env
   (lambda (vals)
     (if (unassigned? (car vals))
         (error "Unbound variable" var)
         (car vals)))
   (lambda () (error "Unbound variable" var))))

; set-variable-value! mutator procedure
(define (set-variable-value! var val env)
  (env-lookup-cps
   var
   env
   (lambda (vals) (set-car! vals val))
   (lambda () (error "Unbound variable -- SET!" var))))

; define-variable! mutator procedure
(define (define-variable! var val env)
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
 | Exercise 4.17
 |
 | Draw diagrams of the environment in effect when evaluating the expression
 | <e3> in the procedure in the text, comparing how this will be structured when
 | definitions are interpreted sequentially with how it will be structured if
 | definitions are scanned out as described. Why is there an extra frame in the
 | transformed program? Explain why this difference in environment structure can
 | never make a difference in the behavior of a correct program. Design a way to
 | make the interpreter implement the "simultaneous" scope rule for internal
 | definitions without constructing the extra frame.
 |
 | Answer
 |
 | In order to evaluate the sequence of expressions ⟨e3⟩, the procedure must be
 | applied to argument values. Let ⟨vars⟩ be the sequence ⟨x₁ x₂ ... xₖ⟩, ⟨args⟩
 | be the sequence of unevaluated expressions ⟨y₁ y₂ ... yₖ⟩, ⟨vals⟩ be the
 | sequence of evaluated expressions of ⟨args⟩ ⟨z₁ z₂ ... zₖ⟩ respectively, and
 | expressions ⟨e1⟩ and ⟨e2⟩ evaluate to values w₁ and w₂ respectively.
 |
 | Option #1: Sequential Evaluation
 |
 |           ┌──────────────────────────────────────────────────────────────┐
 |           │ foo: ...                                                     │
 | global -->│ bar: ...     u: ???                                          │
 |  env      │ baz: ...     v: ???                                          │
 |           └──────────────────────────────────────────────────────────────┘
 |            ((lambda ⟨vars⟩ (define u ⟨e1⟩) (define v ⟨e2⟩) ⟨e3⟩) ⟨args⟩)
 |
 |
 |           ┌──────────────────────────────────────────────────────────────┐
 |           │ foo: ...     ???: ───┐                                       │
 | global -->│ bar: ...     u: ???  │ #[compound-procedure ??]              │
 |  env      │ baz: ...     v: ???  │                                       │
 |           └──────────────────────┼───────────────────────────────────────┘
 |                            ┌─────┘ Λ                   Λ
 |                            V       │                   │
 |                    ┌──────ꙨꙨ───────┘         ┌─────────┴─────────┐
 |                    V                         │ x₁: z₁            │
 |             params: ⟨x₁ x₂ ... xₖ⟩      E1 ->│ x₂: z₂            │
 |             body: (define u ⟨e1⟩)            │ ...               │
 |                   (define v ⟨e2⟩)            │ xₖ: zₖ            │
 |                   ⟨e3⟩                       └───────────────────┘
 |                                                (define u ⟨e1⟩)
 |                                                (define v ⟨e2⟩)
 |                                                ⟨e3⟩
 |
 |
 |           ┌──────────────────────────────────────────────────────────────┐
 |           │ foo: ...     ???: ───┐                                       │
 | global -->│ bar: ...     u: ???  │ #[compound-procedure ??]              │
 |  env      │ baz: ...     v: ???  │                                       │
 |           └──────────────────────┼───────────────────────────────────────┘
 |                            ┌─────┘ Λ                   Λ
 |                            V       │                   │
 |                    ┌──────ꙨꙨ───────┘         ┌─────────┴─────────┐
 |                    V                         │ x₁: z₁   u: w₁    │
 |             params: ⟨x₁ x₂ ... xₖ⟩      E1 ->│ x₂: z₂   v: w₂    │
 |             body: (define u ⟨e1⟩)            │ ...               │
 |                   (define v ⟨e2⟩)            │ xₖ: zₖ            │
 |                   ⟨e3⟩                       └───────────────────┘
 |                                                ⟨e3⟩
 |
 |
 | Option #2: Derived Expression Evaluation
 |
 | Internal Definition Transformation:
 | ((lambda ⟨vars⟩                      ((lambda ⟨vars⟩
 |    (define u ⟨e1⟩)                      (let ((u '*unassigned*)
 |    (define v ⟨e2⟩)           <==>             (v '*unassigned*))
 |    ⟨e3⟩)                                  (set! u ⟨e1⟩)
 |  ⟨args⟩)                                  (set! u ⟨e2⟩)
 |                                           ⟨e3⟩))
 |                                       ⟨args⟩)
 |
 | Derived Expression for let-Expression:
 | (let ((⟨var⟩ ⟨exp⟩)) ⟨body⟩) <==> ((lambda (⟨var⟩) ⟨body⟩) ⟨exp⟩)
 |
 |           ┌──────────────────────────────────────────────────────────────┐
 |           │ foo: ...                                                     │
 | global -->│ bar: ...     u: ???                                          │
 |  env      │ baz: ...     v: ???                                          │
 |           └──────────────────────────────────────────────────────────────┘
 |            ((lambda ⟨vars⟩ (define u ⟨e1⟩) (define v ⟨e2⟩) ⟨e3⟩) ⟨args⟩)
 |
 |
 |           ┌──────────────────────────────────────────────────────────────┐
 |           │ foo: ...     ???: ───┐                                       │
 | global -->│ bar: ...     u: ???  │ #[compound-procedure ??]              │
 |  env      │ baz: ...     v: ???  │                                       │
 |           └──────────────────────┼───────────────────────────────────────┘
 |                            ┌─────┘ Λ                      Λ
 |                            V       │                      │
 |                    ┌──────ꙨꙨ───────┘            ┌─────────┴─────────┐
 |                    V                            │ x₁: z₁            │
 |             params: ⟨x₁ x₂ ... xₖ⟩         E1 ->│ x₂: z₂            │
 |             body: (let ((u '*unassigned*)       │ ...               │
 |                         (v '*unassigned*))      │ xₖ: zₖ            │
 |                     (set! u ⟨e1⟩)               └───────────────────┘
 |                     (set! u ⟨e2⟩)               ((lambda (u v)
 |                     ⟨e3⟩)                          (set! u ⟨e1⟩)
 |                                                    (set! u ⟨e2⟩)
 |                                                    ⟨e3⟩)
 |                                                  '*unassigned*
 |                                                  '*unassigned*)
 |
 |
 |           ┌──────────────────────────────────────────────────────────────┐
 |           │ foo: ...     ???: ───┐                                       │
 | global -->│ bar: ...     u: ???  │ #[compound-procedure ??]              │
 |  env      │ baz: ...     v: ???  │                                       │
 |           └──────────────────────┼───────────────────────────────────────┘
 |                            ┌─────┘ Λ                      Λ
 |                            V       │                      │
 |                    ┌──────ꙨꙨ───────┘            ┌─────────┴─────────┐
 |                    V                            │ x₁: z₁            │
 |             params: ⟨x₁ x₂ ... xₖ⟩         E1 ->│ x₂: z₂            │
 |             body: (let ((u '*unassigned*)       │ ...               │
 |                         (v '*unassigned*))      │ xₖ: zₖ            │
 |                     (set! u ⟨e1⟩)               └───────────────────┘
 |                     (set! u ⟨e2⟩)                         Λ
 |                     ⟨e3⟩)                                 │
 |                                                 ┌─────────┴─────────┐
 |                                                 │ u: '*unassigned*  │
 |                                            E2 ->│ v: '*unassigned*  │
 |                                                 └───────────────────┘
 |                                                    (set! u ⟨e1⟩)
 |                                                    (set! u ⟨e2⟩)
 |                                                    ⟨e3⟩
 |
 |
 |           ┌──────────────────────────────────────────────────────────────┐
 |           │ foo: ...     ???: ───┐                                       │
 | global -->│ bar: ...     u: ???  │ #[compound-procedure ??]              │
 |  env      │ baz: ...     v: ???  │                                       │
 |           └──────────────────────┼───────────────────────────────────────┘
 |                            ┌─────┘ Λ                      Λ
 |                            V       │                      │
 |                    ┌──────ꙨꙨ───────┘            ┌─────────┴─────────┐
 |                    V                            │ x₁: z₁            │
 |             params: ⟨x₁ x₂ ... xₖ⟩         E1 ->│ x₂: z₂            │
 |             body: (let ((u '*unassigned*)       │ ...               │
 |                         (v '*unassigned*))      │ xₖ: zₖ            │
 |                     (set! u ⟨e1⟩)               └───────────────────┘
 |                     (set! u ⟨e2⟩)                         Λ
 |                     ⟨e3⟩)                                 │
 |                                                 ┌─────────┴─────────┐
 |                                                 │ u: w₁             │
 |                                            E2 ->│ v: w₂             │
 |                                                 └───────────────────┘
 |                                                    ⟨e3⟩
 |
 |
 | Q1: Why is there an extra frame in the transformed program?
 |
 | A1: Because the let-expression gets translated into a lambda expression which
 |     creates a new environment frame.
 |
 | Q2: Explain why this difference in environment structure can never make a
 |     difference in the behavior of a correct program.
 |
 | A2: A "correct" program is one where the body of the lambda expression:
 |
 |       1. Is a sequence of expressions where all internal definition
 |          expressions are evaluated before all other expressions.
 |       2. Internal defintions do not reference one another while being
 |          evaluated.
 |
 |     If our program is indeed correct, then expression sequences ⟨e1⟩ and ⟨e2⟩
 |     do not reference u and v directly (i.e., the body of a lambda expression
 |     or a delay expression are acceptable), then sequential evaluation of
 |     internal definitions produces "correct behavior".
 |
 |     Looking at Option #1 for our correct program, we see that the first
 |     extended environment frame E1 contains the mappings (⟨vars⟩: ⟨vals⟩),
 |     (u: w₁), and (v: w₂). The values for u and v shadow/hide those in
 |     environment E0.
 |
 |     Looking at Option #2 for our correct program, we see that the first
 |     extended environment frame E1 contains the mappings (⟨vars⟩: ⟨vals⟩).
 |     The second extended environment frame E2 contains the mappings (u: w₁)
 |     and (v: w₂). The values for u and v shadow/hide those in environment E0.
 |
 |     For both Options, all the free variables in ⟨e3⟩ will have the same
 |     values. Therefore the difference in environment structure can never make
 |     a difference in the behavior of a correct program.
 |
 | Q3: Design a way to make the interpreter implement the "simultaneous" scope
 |     rule for internal definitions without constructing the extra frame.
 |
 | A3: In order to implement "simultaneous" scope rule, we need initializations
 |     ('*unassigned*) to occur before assignments (w₁ and w₂). We can leverage
 |     the sequential evaluation of a lambda expression body by listing the
 |     initializations before the assignments without having to create a new
 |     environment frame.
 |
 |     Transformation:
 |
 |     ((lambda ⟨vars⟩                      ((lambda ⟨vars⟩
 |        (define u ⟨e1⟩)                      (define u '*unassigned*)
 |        (define v ⟨e2⟩)           <==>       (define v '*unassigned*)
 |        ⟨e3⟩)                                (set! u ⟨e1⟩)
 |      ⟨args⟩)                                (set! u ⟨e2⟩)
 |                                             ⟨e3⟩)
 |                                           ⟨args⟩)
 |#
