; §4.1.4 -- must precede def of metacircular apply
; apply-in-underlying-scheme procedure
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

; quoted? predicate procedure
(define (quoted? exp) (tagged-list? exp 'quote))

; text-of-quotation selector procedure
(define (text-of-quotation exp) (cadr exp))

; tagged-list? predicate procedure
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

; variable? predicate procedure
(define (variable? exp) (symbol? exp))

; assignment? predicate procedure
(define (assignment? exp) (tagged-list? exp 'set!))

; assignment-variable selector procedure
(define (assignment-variable exp) (cadr exp))

; assignment-value selector procedure
(define (assignment-value exp) (caddr exp))

; definition? predicate procedure
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
      (make-lambda (cdadr exp)
                   (cddr exp))))

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

; define make-begin constructor procedure
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
(define (cond-else-clause? clause) (eq? (cond-predicate clause) 'else))

; cond-predicate selector procedure
(define (cond-predicate clause) (car clause))

; cond-actions selector procedure
(define (cond-actions clause) (cdr clause))

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
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

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

; the-empty-environment definition
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

; extend-environment procedure
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

; setup-environment procedure
(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

;[do later] (define the-global-environment (setup-environment))

; primitive-procedure? predicate procedure
(define (primitive-procedure? proc) (tagged-list? proc 'primitive))

; primitive-implementation selector procedure
(define (primitive-implementation proc) (cadr proc))

; primitive-procedures list definition
(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
;;      more primitives
        ))

; primitive-procedure-names selector procedure
(define (primitive-procedure-names) (map car primitive-procedures))

; primitive-procedure-objects selector procedure
(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

;[moved to start of file] (define apply-in-underlying-scheme apply)

; apply-primitive-procedure procedure
(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

; input-prompt definition
(define input-prompt ";;; M-Eval input:")

; output-prompt definition
(define output-prompt ";;; M-Eval value:")

; driver-loop procedure
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
(define (announce-output string) (newline) (display string) (newline))

; user-print procedure
(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

;(define the-global-environment (setup-environment))
;(driver-loop)

#|
 | §4.3.3 'Implementing the 'Amb' Evaluator' (modified as needed to run it)
 |#

; amb? predicate procedure
(define (amb? exp) (tagged-list? exp 'amb))

; amb-choices selector procedure
(define (amb-choices exp) (cdr exp))

; analyze procedure from §4.1.6, with clause from §4.3.3 added
; and also support for let expression
(define (analyze exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((let? exp) (analyze (let->combination exp))) ;**
        ((amb? exp) (analyze-amb exp))                ;**
        ((application? exp) (analyze-application exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp))))

; ambeval procedure (CPS)
(define (ambeval exp env succeed fail) ((analyze exp) env succeed fail))

#|
 | Simple expressions
 |#

; analyze-self-evaluating procedure
(define (analyze-self-evaluating exp)
  (lambda (env succeed fail)
    (succeed exp fail)))

; analyze-quoted procedure
(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env succeed fail)
      (succeed qval fail))))

; analyze-variable procedure
(define (analyze-variable exp)
  (lambda (env succeed fail)
    (succeed (lookup-variable-value exp env)
             fail)))

; analyze-lambda procedure
(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env succeed fail)
      (succeed (make-procedure vars bproc env)
               fail))))

#|
 | Conditionals and sequences
 |#

; analyze-if procedure
(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env succeed fail)
      (pproc env
             ;; success continuation for evaluating the predicate
             ;; to obtain pred-value
             (lambda (pred-value fail2)
               (if (true? pred-value)
                   (cproc env succeed fail2)
                   (aproc env succeed fail2)))
             ;; failure continuation for evaluating the predicate
             fail))))

; analyze-sequence procedure
(define (analyze-sequence exps)
  (define (sequentially a b)
    (lambda (env succeed fail)
      (a env
         ;; success continuation for calling a
         (lambda (a-value fail2)
           (b env succeed fail2))
         ;; failure continuation for calling a
         fail)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence -- ANALYZE"))
    (loop (car procs) (cdr procs))))

#|
 | Definitions and assignments
 |#

; analyze-definition procedure
(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (define-variable! var val env)
               (succeed 'ok fail2))
             fail))))

; analyze-assignment procedure
(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)        ; *1*
               (let ((old-value
                      (lookup-variable-value var env)))
                 (set-variable-value! var val env)
                 (succeed 'ok
                          (lambda ()    ; *2*
                            (set-variable-value! var
                                                 old-value
                                                 env)
                            (fail2)))))
             fail))))

#|
 | Procedure applications
 |#

; analyze-application procedure
(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env succeed fail)
      (fproc env
             (lambda (proc fail2)
               (get-args aprocs
                         env
                         (lambda (args fail3)
                           (execute-application
                            proc args succeed fail3))
                         fail2))
             fail))))

; get-args procedure (CPS)
(define (get-args aprocs env succeed fail)
  (if (null? aprocs)
      (succeed '() fail)
      ((car aprocs) env
                    ;; success continuation for this aproc
                    (lambda (arg fail2)
                      (get-args (cdr aprocs)
                                env
                                ;; success continuation for recursive
                                ;; call to get-args
                                (lambda (args fail3)
                                  (succeed (cons arg args)
                                           fail3))
                                fail2))
                    fail)))

; execute-application procedure (CPS)
(define (execute-application proc args succeed fail)
  (cond ((primitive-procedure? proc)
         (succeed (apply-primitive-procedure proc args)
                  fail))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))
          succeed
          fail))
        (else
         (error
          "Unknown procedure type -- EXECUTE-APPLICATION"
          proc))))

#|
 | Evaluating 'amb' expressions
 |#

; analyze-amb procedure
(define (analyze-amb exp)
  (let ((cprocs (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            ((car choices) env
                           succeed
                           (lambda ()
                             (try-next (cdr choices))))))
      (try-next cprocs))))

#|
 | Driver loop
 |#

; input-prompt definition
(define input-prompt ";;; Amb-Eval input:")

; output-prompt definition
(define output-prompt ";;; Amb-Eval value:")

; driver-loop procedure
(define (driver-loop)
  (define (internal-loop try-again)
    (prompt-for-input input-prompt)
    (let ((input (read)))
      (if (eq? input 'try-again)
          (try-again)
          (begin
            (newline)
            (display ";;; Starting a new problem ")
            (ambeval input
                     the-global-environment
                     ;; ambeval success
                     (lambda (val next-alternative)
                       (announce-output output-prompt)
                       (user-print val)
                       (internal-loop next-alternative))
                     ;; ambeval failure
                     (lambda ()
                       (announce-output
                        ";;; There are no more values of")
                       (user-print input)
                       (driver-loop)))))))
  (internal-loop
   (lambda ()
     (newline)
     (display ";;; There is no current problem")
     (driver-loop))))


#|
 | Support for 'let' expression (as noted in footnote 56, p.428)
 |#

; let? predicate procedure
(define (let? exp) (tagged-list? exp 'let))

; let-bindings selector procedure
(define (let-bindings exp) (cadr exp))

; let-body selector procedure
(define (let-body exp) (cddr exp))

; let-var selector procedure
(define (let-var binding) (car binding))

; let-val selector procedure
(define (let-val binding) (cadr binding))

; make-combination constructor procedure
(define (make-combination operator operands) (cons operator operands))

; let->combination procedure
(define (let->combination exp)
  ;;make-combination defined in earlier exercise
  (let ((bindings (let-bindings exp)))
    (make-combination (make-lambda (map let-var bindings)
                                   (let-body exp))
                      (map let-val bindings))))

#|
 | A longer list of primitives -- suitable for running everything in §4.3:
 |
 |   1. Overrides the list in ch4-mceval.scm
 |   2. Has 'not' to support 'require'
 |   3. Various stuff for code in text (including support for 'prime?')
 |   4. 'integer?'
 |   5. 'sqrt' for exercise code
 |   6. 'eq?' for ex. solution
 |#

; primitive-procedures list definition
(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'list list)
        (list 'memq memq)
        (list 'member member)
        (list 'not not)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '= =)
        (list '> >)
        (list '>= >=)
        (list 'abs abs)
        (list 'remainder remainder)
        (list 'integer? integer?)
        (list 'sqrt sqrt)
        (list 'eq? eq?)
;;      more primitives
        ))

(define the-global-environment (setup-environment))
(driver-loop)

(define (require p)
  (if (not p) (amb)))

#|
 | §4.3.2 'Examples of Nondeterministic Programs' -- 'Parsing natural language'
 |#

; nouns list definition
(define nouns '(noun student professor cat class))

; verbs list definition
(define verbs '(verb studies lectures eats sleeps))

; articles list definition
(define articles '(article the a))

; prepositions list definition
(define prepositions '(prep for to in by with))

; conjunctions list definition                                  ;!
(define conjunctions '(conj and but or))

; adjectives list definition                                    ;!
(define adjectives '(adj big small eager lazy))

; adverbs list definition                                       ;!
(define adverbs '(adv loudly quietly))

; *unparsed* mutable "pointer"
(define *unparsed* '())

; parse-word ambiguous procedure
(define (parse-word word-list)
  (require (not (null? *unparsed*)))
  (require (memq (car *unparsed*) (cdr word-list)))
  (let ((found-word (car *unparsed*)))
    (set! *unparsed* (cdr *unparsed*))
    (list (car word-list) found-word)))

; parse abiguous procedure
(define (parse input)
  (set! *unparsed* input)
  (let ((sent (parse-compound-sentence)))
    (require (null? *unparsed*))
    sent))

; parse-prepositional-phrase ambiguous procedure
(define (parse-prepositional-phrase)
  (list 'prep-phrase
        (parse-word prepositions)
        (parse-noun-phrase)))

; parse-simple-sentence ambiguous procedure                     ;!
(define (parse-simple-sentence)
  (list 'simple-sentence
         (parse-noun-phrase)
         (parse-verb-phrase)))

; parse-compound-sentence ambiguous procedure                   ;!
(define (parse-compound-sentence)
  (define (maybe-extend compound-sentence)
    (amb compound-sentence
         (maybe-extend (list 'compound-sentence
                             compound-sentence
                             (parse-word conjunctions)
                             (parse-compound-sentence)))))
  (maybe-extend (parse-simple-sentence)))

; parse-verb-adverb-phrase ambiguous procedure                  ;!
(define (parse-verb-adverb-phrase)
  (amb (parse-word verbs)
       (list 'verb-adverb-phrase
             (parse-word verbs)
             (parse-word adverbs))))

; parse-verb-phrase ambiguous procedure                         ;!
(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (amb verb-phrase
         (maybe-extend (list 'verb-phrase
                             verb-phrase
                             (parse-prepositional-phrase)))))
  (maybe-extend (parse-verb-adverb-phrase)))

; parse-simple-noun-phrase ambiguous procedure                  ;!
(define (parse-simple-noun-phrase)
  (amb (list 'simple-noun-phrase
             (parse-word articles)
             (parse-word adjectives)
             (parse-word nouns))
       (list 'simple-noun-phrase
             (parse-word articles)
             (parse-word nouns))))

; parse-noun-phrase ambiguous procedure
(define (parse-noun-phrase)
  (define (maybe-extend noun-phrase)
    (amb noun-phrase
         (maybe-extend (list 'noun-phrase
                             noun-phrase
                             (parse-prepositional-phrase)))))
  (maybe-extend (parse-simple-noun-phrase)))

#|
 | Exercise 4.48
 |
 | Extend the grammar given above to handle more complex sentences. For
 | example, you could extend noun phrases and verb phrases to include
 | adjectives and adverbs, or you could handle compound sentences.⁵³
 |
 | §4.3.2 Footnote 53
 |
 | This kind of grammar can become arbitrarily complex, but it is only a toy
 | as far as real language understanding is concerned. Real natural-language
 | understanding by computer requires an elaborate mixture of syntactic
 | analysis and interpretation of meaning. On the other hand, even toy parsers
 | can be useful in supporting flexible command languages for programs such as
 | information-retrieval systems. Winston 1992 discusses computational
 | approaches to real language understanding and also the applications of
 | simple grammars to command languages.
 |#

(parse '(The eager professor lectures loudly to the class but a lazy student sleeps))
try-again

(parse '(A student in the class eats and the lazy professor sleeps))
try-again

(parse '(The professor eats or the small cat lectures quietly to the class))
try-again
