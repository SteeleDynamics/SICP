; make-connector constructor procedure
(define (make-connector)
  (let ((value false) (informant false) (constraints '()))
    (define (set-my-value newval setter)
      (cond ((not (has-value? me))
             (set! value newval)
             (set! informant setter)
             (for-each-except setter
                              inform-about-value
                              constraints))
            ((not (= value newval))
             (error "Contradiction" (list value newval)))
            (else 'ignored)))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
          (begin (set! informant false)
                 (for-each-except retractor
                                  inform-about-no-value
                                  constraints))
          'ignored))
    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
          (set! constraints
                (cons new-constraint constraints)))
      (if (has-value? me)
          (inform-about-value new-constraint))
      'done)
    (define (me request)
      (cond ((eq? request 'has-value?)
             (if informant true false))
            ((eq? request 'value) value)
            ((eq? request 'set-value!) set-my-value)
            ((eq? request 'forget) forget-my-value)
            ((eq? request 'connect) connect)
            (else (error "Unknown operation -- CONNECTOR"
                         request))))
    me))

; for-each-except helper procedure
(define (for-each-except exception procedure list)
  (define (loop items)
    (cond ((null? items) 'done)
          ((eq? (car items) exception) (loop (cdr items)))
          (else (procedure (car items))
                (loop (cdr items)))))
  (loop list))

; inform-about-value helper procedure
(define (inform-about-value constraint)
  (constraint 'I-have-a-value))

; inform-about-no-value helper procedure
(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

; has-value? predicate procedure
(define (has-value? connector)
  (connector 'has-value?))

; get-value selector procedure
(define (get-value connector)
  (connector 'value))

; set-value! mutator procedure
(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))

; forget-value! mutator procedure
(define (forget-value! connector retractor)
  ((connector 'forget) retractor))

; connect (mutator?) procedure
(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))

;
;           ┌──────────────────────┐
; global -->│ make-connector: ─┐   │
;  env      └──────────────────┼───┘
;                              │ Λ
;                              V │
;                  ┌──────────ꙨꙨ─┘
;                  V
;       params: -
;       body: (let ((value false) (informant false) (constraints '()))
;               (define (set-my-value newval setter) ...)
;               (define (forget-my-value retractor) ...)
;               (define (connect new-constraint) ...)
;               (define (me request) ...)
;               me)
;
;
;           ┌──────────────────────┐
; global -->│ make-connector: ─┐   │
;  env      └──────────────────┼───┘
;                              │ Λ
;                              V │
;                  ┌──────────ꙨꙨ─┘
;                  V
;       params: -
;       body: ((lambda (value informant constraints)
;                (define (set-my-value newval setter) ...)
;                (define (forget-my-value retractor) ...)
;                (define (connect new-constraint) ...)
;                (define (me request) ...)
;                me)
;              false
;              false
;              '())
;

