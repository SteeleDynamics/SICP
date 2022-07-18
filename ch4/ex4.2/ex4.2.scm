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
 | Exercise 4.2
 |
 | Louis Reasoner plans to reorder the 'cond' clauses in 'eval' so that the
 | clause for procedure applications appears before the clause for assignments.
 | He argues that this will make the interpreter more efficient: Since programs
 | usually contain more applications than assignments, definitions, and so on,
 | his modified 'eval' will usually check fewer clauses than the original
 | 'eval' before identifying the type of an expression.
 |
 | a. What is wrong with Louis's plan? (Hint: What will Louis's evaluator do
 | with the expression '(define x 3)'?)
 |
 | b. Louis is upset that his plan didn't work. He is willing to go to any
 | lengths to make his evaluator recognize procedure applications before it
 | checks for most other kinds of expressions. Help him by changing the syntax
 | of the evaluated language so that procedure applications start with 'call'.
 | For example, instead of '(factorial 3)' we will now have to write
 | 'call factorial 3)' and instead of '(+ 1 2)' we will have to write
 | '(call + 1 2)'.
 |
 | Answer
 |
 |   Part (a)
 |   
 |   The predicate procedure '(define (application? exp) (pair? exp))' only
 |   checks if the expression is a pair. This will match with all of the other
 |   following predicate expressions in the following clauses:
 |
 |   1. 'assignment?'
 |   2. 'definition?'
 |   3. 'if?'
 |   4. 'begin?'
 |   5. 'cond?'
 |
 |   If Louis Reasoner's 'eval' procedure with reordered clauses tries to
 |   evaluate the combination '(define x 3)', it will try to look up the
 |   procedure object value for 'define' in the current environment instead of
 |   mutating the current environment by adding a key-value pair. 'Define'
 |   will no longer be a reserved keyword and 'define'-ing variables and
 |   procedures will be broken. The same misapplication will happen to 'set!',
 |   'if', 'begin', and 'cond'.
 |
 |   Part (b)
 |
 |  (define (application? exp) (tagged-list? exp 'call)) 
 |  (define (operator exp) (cadr exp)) 
 |  (define (operands exp) (cddr exp))
 |#


