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

#|
 | Exercise 4.1
 |
 | Notice that we cannot tell whether the metacircular evaluator evaluates
 | operands from left to right or from right to left. Its evaluation order
 | is inherited from the underlying Lisp: If the arguments to 'cons' in
 | 'list-of-values' are evaluated from left to right, then 'list-of-values'
 | will evaluate operands from left to right; and if the arguments to 'cons'
 | are evaluated from right to left, then 'list-of-values' will evaluate
 | operands from right to left.
 |
 | Write a version of 'list-of-values' that evaluates operands from left to
 | right regardless of the order of evaluation in the underlying Lisp. Also
 | write a version of 'list-of-values' that evaluates operands from right to
 | left.
 |
 | Answer
 |
 | (Applicative-Order) Evaluation of combinations (e₀ e₁ ... eₖ ... eₙ₋₁ eₙ)
 | occurs in the following sequence:
 |
 | 1. Evaluate all subexpressions eₖ IN ANY ORDER to get values vₖ
 | 2. Apply operator procedure value v₀ to operand values v₁ ... vₙ
 |
 | What we know about Lisp/Scheme evaluation is that the expression 
 |
 | (lambda (x) <exp₁>)
 |
 | returns a procedure object value when evaluated. It does not evaluate
 | <exp₁> . Therefore the combination ((lambda (x) <exp₁>) <exp₂>) ensures
 | <exp₂> is evaluated before <exp₁> . This is the exact syntactic
 | sugar used by a 'let' expression.
 |
 | If we nest 'let' expressions such that each contains only one
 | (<varₖ> <expₖ>) name-expression pair, we can enforce the order of
 | evaluation {<exp₁> ==> v1, <exp₂> ==> v2} by the following
 |
 | (let ((v1 <exp₁>))
 |   (let ((v2 <exp₂>))
 |     (cons v1 v2)))
 |#

; list-of-values procedure, left-to-right evaluation
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((first (eval (first-operand exps) env)))
        (let ((rest (list-of-values (rest-of-operands exps) env)))
          (cons first rest)))))

; list-of-values procedure, right-to-left evaluation
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((rest (list-of-values (rest-of-operands exps) env)))
        (let ((first (eval (first-operand exps) env)))
          (cons first rest)))))

; proof-of-concept unit tests
()

; map-left-to-right procedure
(define (map-left-to-right f xs)
  (if (null? xs)
      '()
      (let ((first (f (car xs))))
        (let ((rest (map-left-to-right f (cdr xs))))
          (cons first rest)))))

; map-right-to-left procedure
(define (map-right-to-left f xs)
  (if (null? xs)
      '()
      (let ((rest (map-right-to-left f (cdr xs))))
        (let ((first (f (car xs))))
          (cons first rest)))))

(define xs '(foo bar baz qux))
(map-left-to-right (lambda (x) (begin (newline) (display x) x)) xs)
(map-right-to-left (lambda (x) (begin (newline) (display x) x)) xs)
