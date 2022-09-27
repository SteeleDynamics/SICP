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
        ((letrec? exp) (eval (letrec->let exp) env))
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

; tagged-list? predicate procedure (prefix)
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

; letrec? predicate procedure
(define (letrec? exp) (tagged-list? exp 'letrec))

; letrec-bindings selector procedure
(define (letrec-bindings exp) (cadr exp))

; letrec-body selector procedure
(define (letrec-body exp) (cddr exp))

; letrec-decl procedure
(define (letrec-decl bindings)
  (if (null? bindings)
      '()
      (cons (list (caar bindings) ''*unassigned*) ; note double quote
            (letrec-decl (cdr bindings)))))

; letrec-setv procedure
(define (letrec-setv bindings)
  (if (null? bindings)
      '()
      (cons (cons 'set! (car bindings))
            (letrec-setv (cdr bindings)))))

; letrec->let procedure
(define (letrec->let exp)
  (let ((bindings (letrec-bindings exp))
        (body (letrec-body exp)))
    (let ((decls (letrec-decl bindings))
          (setvs (letrec-setv bindings)))
      (make-let decls (append setvs body)))))

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
  ; (list 'procedure parameters (scan-out-defines body) env))   ;!
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
        (list '< <)
        (list '= =)
        (list '> >)
        (list 'assoc assoc)
        (list 'car car)
        (list 'cadr cadr)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'display display)
        (list 'newline newline)
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
(define the-global-environment (setup-environment))

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
 | §4.2.2 'An Interpreter with Lazy Evaluation - Modifying the evaluator'
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
        ((application? exp)                                     ; clause from
         (apply (actual-value (operator exp) env)               ; book
                (operands exp)
                env))
        (else
         (error "Unknown expression type -- EVAL" exp))))

; actual-value procedure
(define (actual-value exp env)
  (force-it (eval exp env)))

; apply procedure                                               ;!
(define (apply proc arg-exps env)
  (cond ((primitive-procedure? proc)
         (apply-primitive-procedure proc (list-of-arg-values arg-exps env)))
        ((compound-procedure? proc)
         (let ((param-exps (procedure-parameters proc))
               (body (procedure-body proc))
               (penv (procedure-environment proc)))
           (let ((symbols (list-of-param-symbols param-exps))
                 (values (list-of-delayed-args param-exps arg-exps env)))
             (eval-sequence body (extend-environment symbols values penv)))))
        (else
         (error "Unknown procedure type -- APPLY" procedure))))

; two-list? predicate procedure                                 ;!
(define (two-list? exp) (and (list? exp) (= (length exp) 2)))

; valid-tag? predicate procedure                                ;!
(define (valid-tag? exp) (and (not (pair? exp)) (symbol? exp)))

; ape-msg string defn                                           ;!
(define ape-msg "Ambiguous parameter expression -- TAGGED-PARAM?")

; ipe-msg string defn                                           ;!
(define ipe-msg "Invalid parameter expression -- TAGGED-PARAM?")

; ite-msg string defn                                           ;!
(define ite-msg "Invalid tag expression -- TAGGED-PARAM?")

; tagged-param? predicate procedure                             ;!
(define (tagged-param? exp)
  (cond ((symbol? exp) #f)
        ((quoted? exp) (error ape-msg exp))
        ((not (two-list? exp)) (error ipe-msg exp))
        ((not (valid-tag? (cadr exp))) (error ite-msg exp))
        (else exp)))

; delayed-param? predicate procedure                            ;!
(define (delayed-param? exp)
  (let ((res (tagged-param? exp)))
    (cond ((not res) #f)
          ((eq? (cadr res) 'lazy) 'lazy)
          ((eq? (cadr res) 'lazy-memo) 'lazy-memo)
          (else #f))))

; list-of-param-symbols procedure                               ;!
(define (list-of-param-symbols param-exps)
  (cond ((null? param-exps)
         '())
        ((tagged-param? (car param-exps)) => (lambda (res)
         (cons (car res) (list-of-param-symbols (cdr param-exps)))))
        (else
         (cons (car param-exps) (list-of-param-symbols (cdr param-exps))))))

; list-of-arg-vals procedure
(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value (first-operand exps) env)
            (list-of-arg-values (rest-operands exps)
                                env))))

; list-of-delayed-args procedure                                ;!
(define (list-of-delayed-args param-exps arg-exps env)
  (cond ((null? param-exps)
         '())
        ((delayed-param? (car param-exps)) => (lambda (res)
         (cons (delay-it (eq? res 'lazy-memo) (car arg-exps) env)
               (list-of-delayed-args (cdr param-exps) (cdr arg-exps) env))))
        (else
         (cons (actual-value (car arg-exps) env)
               (list-of-delayed-args (cdr param-exps) (cdr arg-exps) env)))))

; eval-if procedure
(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

; input-prompt string
(define input-prompt ";;; L-Eval input:")

; output-prompt string
(define output-prompt ";;; L-Eval value:")

; driver-loop procedure
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output
           (actual-value input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

#|
 | §4.2.2 'An Interpreter with Lazy Evaluation - Representing thunks'
 |#

; delay-it constructor procedure                                ;!
(define (delay-it memo exp env) (list 'thunk memo exp env))

; thunk? predicate procedure
(define (thunk? obj) (tagged-list? obj 'thunk))

; thunk-memo selector procedure                                 ;!
(define (thunk-memo thunk) (cadr thunk))

; thunk-exp selector procedure                                  ;!
(define (thunk-exp thunk) (caddr thunk))

; thunk-env selector procedure                                  ;!
(define (thunk-env thunk) (cadddr thunk))

; evaluated-thunk? predicate procedure
(define (evaluated-thunk? obj) (tagged-list? obj 'evaluated-thunk))

; thunk-value selector procedure                                ;!
(define (thunk-value evaluated-thunk) (caddr evaluated-thunk))

; force-it procedure (with and without memoization)             ;!
(define (force-it obj)
  (cond ((and (thunk? obj) (thunk-memo obj))                    ;!
         (let ((result (actual-value (thunk-exp obj) (thunk-env obj))))
           (set-car! obj 'evaluated-thunk)
           (set-car! (cddr obj) result)  ; replace exp with val ;!
           (set-cdr! (cddr obj) '())     ; forget unneeded env  ;!
           result))
        ((and (thunk? obj) (not (thunk-memo obj)))              ;!
         (actual-value (thunk-exp obj) (thunk-env obj)))        ;!
        ((evaluated-thunk? obj)
         (thunk-value obj))
        (else obj)))

#|
 | Exercise 4.31
 |
 | The approach taken in this section is somewhat unpleasant, because it makes
 | an incompatible change to Scheme. It might be nicer to implement lazy
 | evaluation as an *upward-compatible extension*, that is, so that ordinary
 | Scheme programs will work as before. We can do this by extending the syntax
 | of procedure declarations to let the user control whether or not arguments
 | are to be delayed. While we're at it, we may as well also give the user the
 | choice between delaying with and without memoization. For example, the
 | definition
 |
 | (define (f a (b lazy) c (d lazy-memo))
 |   ...)
 |
 | would define 'f' to be a procedure of four arguments, where the first and
 | third arguments are evaluated when the procedure is called, the second
 | argument is delayed, and the fourth argument is both delayed and memoized.
 | Thus, ordinary procedure definitions will produce the same behavior as
 | ordinary Scheme, while adding the 'lazy-memo' declaration to each parameter
 | of every compound procedure will produce the behavior of the lazy evaluator
 | defined in this section. Design and implement the changes required to produce
 | such an extension to Scheme. You will have to implement new syntax procedures
 | to handle the new syntax for 'define'. You must also arrange for 'eval' or
 | 'apply' to determine when arguments are to be delayed, and to force or delay
 | arguments accordingly, and you must arrange for forcing to memoize or not, as
 | appropriate.
 |#

; tagged-param? unit tests
(tagged-param? 'lazy)
(tagged-param? ''lazy)
(restart 1)
(tagged-param? '(quote lazy))
(restart 1)
(tagged-param? '"lazy")
(restart 1)
(tagged-param? '42)
(restart 1)
(tagged-param? (cons 'x 'lazy))
(restart 1)
(tagged-param? '(x . lazy))
(restart 1)
(tagged-param? '())
(restart 1)
(tagged-param? '(lazy))
(restart 1)
(tagged-param? '(x lazy lazy))
(restart 1)
(tagged-param? '(x 'lazy))
(restart 1)
(tagged-param? '(x "lazy"))
(restart 1)
(tagged-param? '(x 42))
(restart 1)
(tagged-param? '(x lazy))

; receiver procedure
(define (receiver run-time gc-time real-time)
    (newline)
    (write (internal-time/ticks->seconds run-time))
    (write-char #\space)
    (write (internal-time/ticks->seconds gc-time))
    (write-char #\space)
    (write (internal-time/ticks->seconds real-time)))

; expressions to evaluate
(define exp0
  '(define (f n) (if (< n 2) n (+ (f (- n 1)) (f (- n 2))))))
(define exp1
  '(define (g (n lazy)) (if (< n 2) n (+ (g (- n 1)) (g (- n 2))))))
(define exp2
  '(define (h (n lazy-memo)) (if (< n 2) n (+ (h (- n 1)) (h (- n 2))))))
(define exp3 '(f 17))
(define exp4 '(g 17))
(define exp5 '(h 17))

; evaluate expressions in evaluator
(define E0 (setup-environment))
(actual-value exp0 E0)
(actual-value exp1 E0)
(actual-value exp2 E0)
(with-timings (lambda () (actual-value exp3 E0)) receiver)
(with-timings (lambda () (actual-value exp4 E0)) receiver)
(with-timings (lambda () (actual-value exp5 E0)) receiver)
