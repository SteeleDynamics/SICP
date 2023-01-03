#|
 | §4.4.4.1 - The Driver Loop and Instantiation
 |#

; input-prompt string definition
(define input-prompt ";;; Query input:")

; output-prompt string definition
(define output-prompt ";;; Query results:")

; query-driver-loop procedure
(define (query-driver-loop)
  (prompt-for-input input-prompt)
  (let ((q (query-syntax-process (read))))
    (let ((stream (singleton-stream (make-frame '() '()))))
      (cond ((assertion-to-be-added? q)
             (add-rule-or-assertion! (add-assertion-body q))
             (newline)
             (display "Assertion added to data base.")
             (query-driver-loop))
            (else
             (newline)
             (display output-prompt)
             ;; [extra newline at end] (announce-output output-prompt)
             (display-stream
              (stream-map
               (lambda (frame)
                 (instantiate q
                              frame
                              (lambda (v f)
                                (contract-question-mark v))))
               (stream-eval-promises true (qeval q stream '()))))   ; ***
             (query-driver-loop))))))

; instantiate procedure
(define (instantiate exp frame unbound-var-handler)
  (define (copy exp)
    (cond ((var? exp)
           (let ((binding (binding-in-frame exp frame)))
             (if binding
                 (copy (binding-value binding))
                 (unbound-var-handler exp frame))))
          ((pair? exp)
           (cons (copy (car exp)) (copy (cdr exp))))
          (else exp)))
  (copy exp))

#|
 | §4.4.4.2 - The Evaluator
 |#

; qeval procedure
(define (qeval query frame-stream hist)                             ; ***
  (let ((qproc (get (type query) 'qeval)))
    (if qproc
        (qproc (contents query) frame-stream hist)
        (simple-query query frame-stream hist))))

; simple-query procedure
(define (simple-query query-pattern frame-stream hist)              ; ***
  (stream-flatmap
   (lambda (frame)
     (stream-append-delayed
      (find-assertions query-pattern frame)
      (delay (apply-rules query-pattern frame hist))))
   frame-stream))

; conjoin procedure
(define (conjoin conjuncts frame-stream hist)                       ; ***
  (if (empty-conjunction? conjuncts)
      frame-stream
      (let ((first (first-conjunct conjuncts))
            (rest (rest-conjuncts conjuncts)))
        (let ((result (qeval first frame-stream hist)))
          (conjoin rest result hist)))))

;;(put 'and 'qeval conjoin)

; disjoin procedure
(define (disjoin disjuncts frame-stream hist)                       ; ***
  (if (empty-disjunction? disjuncts)
      the-empty-stream
      (let ((first (first-disjunct disjuncts))
            (rest (rest-disjuncts disjuncts)))
        (interleave-delayed
         (qeval first frame-stream hist)
         (delay (disjoin rest frame-stream hist))))))

;;(put 'or 'qeval disjoin)

; negate procedure
(define (negate operands frame-stream hist)                         ; ***
  (stream-add-or-eval-promise                                       ; ***
   (cons-promise
    operands
    (lambda (frame)
      (stream-null?
       (qeval (negated-query operands)
              (singleton-stream frame)
              hist))))
   frame-stream))

;;(put 'not 'qeval negate)

; lisp-value procedure
(define (lisp-value call frame-stream hist)                         ; ***
  (stream-add-or-eval-promise                                       ; ***
   (cons-promise
    call
    (lambda (frame)
      (execute
       (instantiate
        call
        frame
        (lambda (v f)
          (error "Unknown pat var -- LISP-VALUE" v))))))
   frame-stream))

;;(put 'lisp-value 'qeval lisp-value)

; execute procedure
(define (execute exp)
  (apply (eval (predicate exp) user-initial-environment)
         (args exp)))

; always-true procedure                                             ; ***
(define (always-true ignore frame-stream hist) frame-stream)

;;(put 'always-true 'qeval always-true)

; uniquely-asserted procedure
(define (uniquely-asserted contents frame-stream hist)              ; ***
  (stream-add-or-eval-promise                                       ; ***
   (cons-promise
    contents
    (lambda (frame)
      (let ((temp-1 (unique-query contents))
            (temp-2 (singleton-stream frame)))
        (let ((result (qeval temp-1 temp-2 hist)))
          (if (singleton-stream? result)
              (stream-car result)
              false)))))
   frame-stream))

; singleton-stream? predicate procedure
(define (singleton-stream? stream)                                  ; ***
  (and (not (stream-null? stream))
       (stream-null? (stream-cdr stream))))

;;(put 'unique 'qeval uniquely-asserted)

; eval-promises procedure
(define (eval-promises forced frame)                                ; ***
  (let ((frame-without-promises (make-frame (frame-bindings frame) '())))
    (define (iter promises acc)
      (if (empty-promise? promises)
          (make-frame (frame-bindings frame) acc)
          (let ((first (first-promise promises))
                (rest (rest-promises promises)))
            (if (or forced
                    (not (unbound-var? (promise-exp first) frame)))
                (let ((pred (promise-pred first)))
                  (if (pred frame-without-promises)
                      (iter rest acc)
                      'failed))
                (iter rest (extend-promises first acc))))))
    (iter (frame-promises frame) '())))

; stream-eval-promises procedure                                    ; ***
(define (stream-eval-promises forced frame-stream)
  (simple-stream-flatmap
   (lambda (frame)
     (let ((result (eval-promises forced frame)))
       (if (eq? 'failed result)
           the-empty-stream
           (singleton-stream result))))
   frame-stream))

; stream-add-or-eval-promises                                       ; ***
(define (stream-add-or-eval-promise promise frame-stream)
  (stream-eval-promises
   false
   (stream-map
    (lambda (frame)
      (make-frame
       (frame-bindings frame)
       (extend-promises promise (frame-promises frame))))
    frame-stream)))

#|
 | §4.4.4.3 - Finding Assertions by Pattern Matching
 |#

; find-assertions procedure
(define (find-assertions pattern frame)
  (simple-stream-flatmap (lambda (datum)                            ; ***
                           (check-an-assertion datum pattern frame))
                         (fetch-assertions pattern frame)))

; check-an-assertion procedure
(define (check-an-assertion assertion query-pat query-frame)
  (let ((match-result
         (pattern-match query-pat assertion query-frame)))
    (if (eq? match-result 'failed)
        the-empty-stream
        (singleton-stream match-result))))

; pattern-match procedure
(define (pattern-match pat dat frame)
  (cond ((eq? frame 'failed) 'failed)
        ((equal? pat dat) frame)
        ((var? pat) (extend-if-consistent pat dat frame))
        ((and (pair? pat) (pair? dat))
         (pattern-match (cdr pat)
                        (cdr dat)
                        (pattern-match (car pat)
                                       (car dat)
                                       frame)))
        (else 'failed)))

; extend-if-consistent procedure
(define (extend-if-consistent var dat frame)
  (let ((binding (binding-in-frame var frame)))
    (if binding
        (pattern-match (binding-value binding) dat frame)
        (extend var dat frame))))

#|
 | §4.4.4.4 - Rules and Unification
 |#

; apply-rules procedure
(define (apply-rules pattern frame hist)                            ; ***
  (stream-flatmap (lambda (rule)
                    (apply-a-rule rule pattern frame hist))
                  (fetch-rules pattern frame)))

; apply-a-rule procedure
(define (apply-a-rule rule query-pattern query-frame hist)          ; ***
  (let ((clean-rule (rename-variables-in rule)))
    (let ((pattern query-pattern)
          (conclusion (conclusion clean-rule))
          (body (rule-body clean-rule))
          (frame query-frame))
      (let ((unify-result (unify-match pattern conclusion frame)))
        (let ((norm-result (norm-inst pattern unify-result))
              (frame-stream (singleton-stream unify-result)))
          (cond ((eq? norm-result 'failed) the-empty-stream)
                ((member norm-result hist)
                 (newline)
                 (display "⟨infinite loop⟩")
                 the-empty-stream)
                (else
                 (qeval body frame-stream (cons norm-result hist)))))))))

; norm-inst procedure
(define (norm-inst exp unified-frame)                               ; ***
  (let ((unbound-var-handler (lambda (v f) (norm-question-mark v))))
    (cond ((eq? unified-frame 'failed) 'failed)
          (else (instantiate exp unified-frame unbound-var-handler)))))

; rename-variables-in procedure
(define (rename-variables-in rule)
  (let ((rule-application-id (new-rule-application-id)))
    (define (tree-walk exp)
      (cond ((var? exp)
             (make-new-variable exp rule-application-id))
            ((pair? exp)
             (cons (tree-walk (car exp))
                   (tree-walk (cdr exp))))
            (else exp)))
    (tree-walk rule)))

; unify-match procedure
(define (unify-match p1 p2 frame)
  (cond ((eq? frame 'failed) 'failed)
        ((equal? p1 p2) frame)
        ((var? p1) (extend-if-possible p1 p2 frame))
        ((var? p2) (extend-if-possible p2 p1 frame)) ; ***
        ((and (pair? p1) (pair? p2))
         (unify-match (cdr p1)
                      (cdr p2)
                      (unify-match (car p1)
                                   (car p2)
                                   frame)))
        (else 'failed)))

; extend-if-possible procedure
(define (extend-if-possible var val frame)
  (let ((binding (binding-in-frame var frame)))
    (cond (binding
           (unify-match
            (binding-value binding) val frame))
          ((var? val)                     ; ***
           (let ((binding (binding-in-frame val frame)))
             (if binding
                 (unify-match
                  var (binding-value binding) frame)
                 (extend var val frame))))
          ((depends-on? val var frame)    ; ***
           'failed)
          (else (extend var val frame)))))

; depends-on? predicate procedure
(define (depends-on? exp var frame)
  (define (tree-walk e)
    (cond ((var? e)
           (if (equal? var e)
               true
               (let ((b (binding-in-frame e frame)))
                 (if b
                     (tree-walk (binding-value b))
                     false))))
          ((pair? e)
           (or (tree-walk (car e))
               (tree-walk (cdr e))))
          (else false)))
  (tree-walk exp))

#|
 | §4.4.4.5 - Maintaining the Data Base
 |#

; THE-ASSERTIONS stream definition
(define THE-ASSERTIONS the-empty-stream)

; fetch-assertions procedure
(define (fetch-assertions pattern frame)
  (if (use-index? pattern)
      (get-indexed-assertions pattern)
      (get-all-assertions)))

; get-all-assertions procedure
(define (get-all-assertions) THE-ASSERTIONS)

; get-indexed-assertions procedure
(define (get-indexed-assertions pattern)
  (get-stream (index-key-of pattern) 'assertion-stream))

; get-stream procedure
(define (get-stream key1 key2)
  (let ((s (get key1 key2)))
    (if s s the-empty-stream)))

; THE-RULES stream definition
(define THE-RULES the-empty-stream)

; fetch-rules procedure
(define (fetch-rules pattern frame)
  (if (use-index? pattern)
      (get-indexed-rules pattern)
      (get-all-rules)))

; get-all-rules procedure
(define (get-all-rules) THE-RULES)

; get-indexed-rules procedure
(define (get-indexed-rules pattern)
  (stream-append
   (get-stream (index-key-of pattern) 'rule-stream)
   (get-stream '? 'rule-stream)))

; add-rule-or-assertion! mutator procedure
(define (add-rule-or-assertion! assertion)
  (if (rule? assertion)
      (add-rule! assertion)
      (add-assertion! assertion)))

; add-assertion! mutator procedure
(define (add-assertion! assertion)
  (store-assertion-in-index assertion)
  (let ((old-assertions THE-ASSERTIONS))
    (set! THE-ASSERTIONS
          (cons-stream assertion old-assertions))
    'ok))

; add-rule! mutator procedure
(define (add-rule! rule)
  (store-rule-in-index rule)
  (let ((old-rules THE-RULES))
    (set! THE-RULES (cons-stream rule old-rules))
    'ok))

; store-assertion-in-index procedure
(define (store-assertion-in-index assertion)
  (if (indexable? assertion)
      (let ((key (index-key-of assertion)))
        (let ((current-assertion-stream
               (get-stream key 'assertion-stream)))
          (put key
               'assertion-stream
               (cons-stream assertion
                            current-assertion-stream))))))

; store-rule-in-index procedure
(define (store-rule-in-index rule)
  (let ((pattern (conclusion rule)))
    (if (indexable? pattern)
        (let ((key (index-key-of pattern)))
          (let ((current-rule-stream
                 (get-stream key 'rule-stream)))
            (put key
                 'rule-stream
                 (cons-stream rule
                              current-rule-stream)))))))

; indexable? predicate procedure
(define (indexable? pat) (or (constant-symbol? (car pat)) (var? (car pat))))

; index-key-of procedure
(define (index-key-of pat)
  (let ((key (car pat)))
    (if (var? key) '? key)))

; use-index? predicate procedure
(define (use-index? pat) (constant-symbol? (car pat)))

#|
 | §4.4.4.6 - Stream operations
 |#

; stream-append-delayed procedure
(define (stream-append-delayed s1 delayed-s2)
  (if (stream-null? s1)
      (force delayed-s2)
      (cons-stream
       (stream-car s1)
       (stream-append-delayed (stream-cdr s1) delayed-s2))))

; interleave-delayed procedure
(define (interleave-delayed s1 delayed-s2)
  (if (stream-null? s1)
      (force delayed-s2)
      (cons-stream
       (stream-car s1)
       (interleave-delayed (force delayed-s2)
                           (delay (stream-cdr s1))))))

; stream-flatmap procedure
(define (stream-flatmap proc s) (flatten-stream (stream-map proc s)))

; flatten-stream procedure
(define (flatten-stream stream)
  (if (stream-null? stream)
      the-empty-stream
      (interleave-delayed
       (stream-car stream)
       (delay (flatten-stream (stream-cdr stream))))))

; singleton-stream constructor procedure
(define (singleton-stream x) (cons-stream x the-empty-stream))

; simple-stream-flatmap procedure
(define (simple-stream-flatmap proc s)                              ; ***
  (simple-flatten (stream-map proc s)))

; simple-flatten procedure
(define (simple-flatten stream)                                     ; ***
  (stream-map stream-car
              (stream-filter
               (lambda (s) (not (stream-null? s)))
               stream)))

#|
 | §4.4.4.7 - Query syntax procedures
 |#

; type selector procedure
(define (type exp)
  (if (pair? exp)
      (car exp)
      (error "Unknown expression TYPE" exp)))

; contents selector procedure
(define (contents exp)
  (if (pair? exp)
      (cdr exp)
      (error "Unknown expression CONTENTS" exp)))

; assertion-to-be-added? predicate procedure
(define (assertion-to-be-added? exp) (eq? (type exp) 'assert!))

; add-assertion-body selector procedure
(define (add-assertion-body exp) (car (contents exp)))

; empty-conjunction? predicate procedure
(define (empty-conjunction? exps) (null? exps))

; first-conjunct selector procedure
(define (first-conjunct exps) (car exps))

; rest-conjuncts selector procedure
(define (rest-conjuncts exps) (cdr exps))

; empty-conjunction? predicate procedure
(define (empty-disjunction? exps) (null? exps))

; first-disjunct selector procedure
(define (first-disjunct exps) (car exps))

; rest-disjuncts selector procedure
(define (rest-disjuncts exps) (cdr exps))

; negated-query selector procedure
(define (negated-query exps) (car exps))

; predicate selector procedure
(define (predicate exps) (car exps))

; args selector procedure
(define (args exps) (cdr exps))

; rule? predicate procedure
(define (rule? statement) (tagged-list? statement 'rule))

; conclusion selector procedure
(define (conclusion rule) (cadr rule))

; rule-body selector procedure
(define (rule-body rule)
  (if (null? (cddr rule))
      '(always-true)
      (caddr rule)))

; unique-query selector procedure                                   ; ***
(define (unique-query exps) (car exps))

; query-syntax-process procedure
(define (query-syntax-process exp) (map-over-symbols expand-question-mark exp))

; map-over-symbols process
(define (map-over-symbols proc exp)
  (cond ((pair? exp)
         (cons (map-over-symbols proc (car exp))
               (map-over-symbols proc (cdr exp))))
        ((symbol? exp) (proc exp))
        (else exp)))

; expand-question-mark procedure
(define (expand-question-mark symbol)
  (let ((chars (symbol->string symbol)))
    (if (string=? (substring chars 0 1) "?")
        (list '?
              (string->symbol
               (substring chars 1 (string-length chars))))
        symbol)))

; var? predicate procedure
(define (var? exp) (tagged-list? exp '?))

; constant-symbol? predicate procedure
(define (constant-symbol? exp) (symbol? exp))

; rule-counter definition
(define rule-counter 0)

; new-rule-application-id procedure (mutator)
(define (new-rule-application-id)
  (set! rule-counter (+ 1 rule-counter))
  rule-counter)

; make-new-variable constructor procedure
(define (make-new-variable var rule-application-id)
  (cons '? (cons rule-application-id (cdr var))))

; contract-question-mark procedure
(define (contract-question-mark variable)
  (string->symbol
   (string-append "?"
     (if (number? (cadr variable))
         (string-append (symbol->string (caddr variable))
                        "-"
                        (number->string (cadr variable)))
         (symbol->string (cadr variable))))))

; norm-question-mark procedure
(define (norm-question-mark variable)                               ; ***
  (string->symbol
   (string-append "?"
    (symbol->string
     (if (number? (cadr variable))
         (caddr variable)
         (cadr variable))))))

#|
 | §4.4.4.8 - Frames and bindings
 |#

; make-binding constructor procedure
(define (make-binding variable value) (cons variable value))

; binding-variable selector procedure
(define (binding-variable binding) (car binding))

; binding-value selector procedure
(define (binding-value binding) (cdr binding))

; extend-bindings procedure                                         ; ***
(define (extend-bindings binding bindings) (cons binding bindings))

; cons-promise constructor procedure (make-promise name-collision)  ; ***
(define (cons-promise exp pred) (cons exp pred))

; promise-exp selector procedure                                    ; ***
(define (promise-exp promise) (car promise))

; promise-pred selector procedure                                   ; ***
(define (promise-pred promise) (cdr promise))

; extend-promises procedure                                         ; ***
(define (extend-promises promise promises) (cons promise promises))

; empty-promise? predicate procedure                                ; ***
(define (empty-promise? promises) (null? promises))

; first-promise selector procedure                                  ; ***
(define (first-promise promises) (car promises))

; rest-promises selector procedure                                  ; ***
(define (rest-promises promises) (cdr promises))

; make-frame constructor procedure                                  ; ***
(define (make-frame bindings promises) (cons bindings promises))

; frame-bindings selector procedure                                 ; ***
(define (frame-bindings frame) (car frame))

; frame-promises selector procedure                                 ; ***
(define (frame-promises frame) (cdr frame))

; binding-in-frame procedure
(define (binding-in-frame variable frame)                           ; ***
  (assoc variable (frame-bindings frame)))

; extend procedure
(define (extend variable value frame)                               ; ***
  (eval-promises
   false
   (make-frame
    (extend-bindings (make-binding variable value) (frame-bindings frame))
    (frame-promises frame))))

; unbound-var? predicate procedure
(define (unbound-var? exp frame)                                    ; ***
  (cond ((var? exp)
         (let ((binding (binding-in-frame exp frame)))
           (if binding
               (unbound-var? (binding-value binding) frame)
               true)))
        ((pair? exp)
         (or (unbound-var? (car exp) frame)
             (unbound-var? (cdr exp) frame)))
        (else false)))

#|
 | From §4.1
 |#

; tagged-list procedure
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

; prompt-for-input procedure
(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

#|
 | Stream support from §3
 |#

; stream-map procedure
(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

; stream-for-each procedure
(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

; display-stream procedure
(define (display-stream s)
  (stream-for-each display-line s))

; display-line procedure
(define (display-line x)
  (newline)
  (display x))

; stream-filter procedure
(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

; stream-append procedure
(define (stream-append s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (stream-append (stream-cdr s1) s2))))

; interleave procedure
(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

#|
 | Table support from §3.3.3 (local tables)
 |#

; make-table constructor procedure
(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

#|
 | From Instructor's Manual
 |#

; define get list
(define get '())

; define put list
(define put '())

; initialize-data-base procedure
(define (initialize-data-base rules-and-assertions)
  (define (deal-out r-and-a rules assertions)
    (cond ((null? r-and-a)
           (set! THE-ASSERTIONS (list->stream assertions))
           (set! THE-RULES (list->stream rules))
           'done)
          (else
           (let ((s (query-syntax-process (car r-and-a))))
             (cond ((rule? s)
                    (store-rule-in-index s)
                    (deal-out (cdr r-and-a)
                              (cons s rules)
                              assertions))
                   (else
                    (store-assertion-in-index s)
                    (deal-out (cdr r-and-a)
                              rules
                              (cons s assertions))))))))
  (let ((operation-table (make-table)))
    (set! get (operation-table 'lookup-proc))
    (set! put (operation-table 'insert-proc!)))
  (put 'and 'qeval conjoin)
  (put 'or 'qeval disjoin)
  (put 'not 'qeval negate)
  (put 'lisp-value 'qeval lisp-value)
  (put 'always-true 'qeval always-true)
  (put 'unique 'qeval uniquely-asserted)                            ; ***
  (deal-out rules-and-assertions '() '()))

#|
 | Do following to reinit the data base from microshaft-data-base in Scheme
 | (not in the query driver loop):
 |
 | (initialize-data-base microshaft-data-base)
 |#

; microshaft-data-base list definition
(define microshaft-data-base
  '(; Ben Bitdiddle
    (address (Bitdiddle Ben) (Slumerville (Ridge Road) 10))
    (job (Bitdiddle Ben) (computer wizard))
    (salary (Bitdiddle Ben) 60000)
    (supervisor (Bitdiddle Ben) (Warbucks Oliver))
    ; Alyssa P. Hacker
    (address (Hacker Alyssa P) (Cambridge (Mass Ave) 78))
    (job (Hacker Alyssa P) (computer programmer))
    (salary (Hacker Alyssa P) 40000)
    (supervisor (Hacker Alyssa P) (Bitdiddle Ben))
    ; Cy D. Fect
    (address (Fect Cy D) (Cambridge (Ames Street) 3))
    (job (Fect Cy D) (computer programmer))
    (salary (Fect Cy D) 35000)
    (supervisor (Fect Cy D) (Bitdiddle Ben))
    ; Lem E. Tweakit
    (address (Tweakit Lem E) (Boston (Bay State Road) 22))
    (job (Tweakit Lem E) (computer technician))
    (salary (Tweakit Lem E) 25000)
    (supervisor (Tweakit Lem E) (Bitdiddle Ben))
    ; Louis Reasoner
    (address (Reasoner Louis) (Slumerville (Pine Tree Road) 80))
    (job (Reasoner Louis) (computer programmer trainee))
    (salary (Reasoner Louis) 30000)
    (supervisor (Reasoner Louis) (Hacker Alyssa P))
    ; Oliver Warbucks
    (address (Warbucks Oliver) (Swellesley (Top Heap Road)))
    (job (Warbucks Oliver) (administration big wheel))
    (salary (Warbucks Oliver) 150000)
    ; Eben Scrooge
    (address (Scrooge Eben) (Weston (Shady Lane) 10))
    (job (Scrooge Eben) (accounting chief accountant))
    (salary (Scrooge Eben) 75000)
    (supervisor (Scrooge Eben) (Warbucks Oliver))
    ; Robert Cratchet
    (address (Cratchet Robert) (Allston (N Harvard Street) 16))
    (job (Cratchet Robert) (accounting scrivener))
    (salary (Cratchet Robert) 18000)
    (supervisor (Cratchet Robert) (Scrooge Eben))
    ; DeWitt Aull
    (address (Aull DeWitt) (Slumerville (Onion Square) 5))
    (job (Aull DeWitt) (administration secretary))
    (salary (Aull DeWitt) 25000)
    (supervisor (Aull DeWitt) (Warbucks Oliver))
    ; can-do-job assertions
    (can-do-job (computer wizard) (computer programmer))
    (can-do-job (computer wizard) (computer technician))
    (can-do-job (computer programmer) (computer programmer trainee))
    (can-do-job (administration secretary) (administration big wheel))
    ; lives-near rule
    (rule (lives-near ?person-1 ?person-2)
          (and (address ?person-1 (?town . ?rest-1))
               (address ?person-2 (?town . ?rest-2))
               (not (same ?person-1 ?person-2))))
    ; same rule
    (rule (same ?x ?x))
    ; wheel rule
    (rule (wheel ?person)
          (and (supervisor ?middle-manager ?person)
               (supervisor ?x ?middle-manager)))
    ; outranked-by rule
    (rule (outranked-by ?staff-person ?boss)
          (or (supervisor ?staff-person ?boss)
              (and (supervisor ?staff-person ?middle-manager)
                   (outranked-by ?middle-manager ?boss))))
    ; Genesis 4 assertions
    (son Adam Cain)
    (son Cain Enoch)
    (son Enoch Irad)
    (son Irad Mehujael)
    (son Mehujael Methushael)
    (son Methushael Lamech)
    (wife Lamech Ada)
    (son Ada Jabal)
    (son Ada Jubal)
    ))

#|
 | Exercise 4.77
 |
 | In section 4.4.3 we saw that 'not' and 'lisp-value' can cause the
 | query language to give "wrong" answers if these filtering operations
 | are applied to frames in which variables are unbound. Devise a way
 | to fix this shortcoming. One idea is to perform the filtering in a
 | "delayed" manner by appending to the frame a "promise" to filter that
 | is fulfilled only when enough variables have been bound to make the
 | operation possible. We could wait to perform filtering until all other
 | operations have been performed. However, for efficiency's sake, we
 | would like to perform filtering as soon as possible so as to cut down
 | on the number of intermediate frames
 |
 | Tasks:
 |
 | 1. Implement ⟨promise⟩ selector and constructor procedures. (*DONE*)
 |    a. 'make-promise' (*DONE*)
 |    b. 'promise-exp' (*DONE*)
 |    c. 'promise-pred' (*DONE*)
 |    d. 'extend-promises' (*DONE*)
 |    e. 'empty-promise?' (*DONE*)
 |    f. 'first-promise' (*DONE*)
 |    g. 'rest-promises' (*DONE*)
 |
 | 2. Augment ⟨binding⟩ selector and constructor procedures. (*DONE*)
 |    a. 'make-binding' (*DONE*)
 |    b. 'binding-variable' (*DONE*)
 |    c. 'binding-value' (*DONE*)
 |    d. 'extend-bindings' (*DONE*)
 |
 | 3. Implement '(unbound-var? ⟨exp⟩ ⟨frame⟩)' procedure. (*DONE*)
 |
 | 4. Implement 'eval-promises' procedure. (*DONE*)
 |
 | 5. Augment ⟨frame⟩ selector and constructor procedures. (*DONE*)
 |    a. 'make-frame' (*DONE*)
 |    b. 'frame-bindings' (*DONE*)
 |    c. 'frame-promises' (*DONE*)
 |    d. 'binding-in-frame' (*DONE*)
 |    e. 'extend' (*DONE*)
 |
 | 6. Augment 'negate', 'lisp-value', 'uniquely-asserted' (*DONE*)
 |    a. Implement 'stream-eval-promises' procedure (*DONE*)
 |    b. Implement 'stream-add-or-eval-promises' procedure (*DONE*)
 |    c. Augment procs by using 'stream-add-or-eval-promises' (*DONE*)
 |#

(initialize-data-base microshaft-data-base)
(query-driver-loop)

(assert! (rule (grandson ?g ?s) (and (son ?f ?s) (son ?g ?f))))
(assert! (rule (son ?m ?s) (and (wife ?m ?w) (son ?w ?s))))

(assert! (rule ((grandson) ?x ?y) (grandson ?x ?y)))
(assert! (rule ((great . (?u . ?v)) ?x ?y)
               (and (son ?x ?z) ((?u . ?v) ?z ?y))))

((great grandson) ?g ?ggs)
(?relationship Adam Irad)
(?relationship Adam Jabal)
(?relationship Adam Jubal)
(?relationship Cain Lamech)

(assert! (rule (append-to-form () ?y ?y)))
(assert! (rule (append-to-form (?u . ?v) ?y (?u . ?z))
               (append-to-form ?v ?y ?z)))

(append-to-form (a b) (c d) ?z)
(append-to-form (a b) ?y (a b c d))
(append-to-form ?x ?y (a b c d))

(assert! (rule (reverse '() '())))
(assert! (rule (reverse (?x) (?x))))
(assert! (rule (reverse (?x . ?y) ?z)
               (and (reverse ?y ?w)
                    (append-to-form ?w (?x) ?z))))

(reverse (1 2 3) ?x)
(reverse ?x (1 2 3))

(unique (job ?x (computer wizard)))
(unique (job ?x (computer programmer)))
(and (job ?x ?j) (unique (job ?anyone ?j)))
(and (job ?x ?j) (unique (supervisor ?y ?x)))

(and (supervisor ?x ?y) (not (job ?x (computer programmer))))
(and (not (job ?x (computer programmer))) (supervisor ?x ?y))

(and (salary (Bitdiddle Ben) ?a) (salary ?x ?b) (lisp-value < ?b ?a))
(and (salary (Bitdiddle Ben) ?a) (lisp-value < ?b ?a) (salary ?x ?b))

(and (unique (job ?anyone ?j)) (job ?x ?j))
(and (unique (supervisor ?y ?x)) (job ?x ?j))
