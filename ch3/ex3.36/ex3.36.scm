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
;           ┌───────────────────────────────────────────────────────────────┐
;           │ inform-about-value: ...       set-value!: ...                 │
;           │ inform-about-no-value: ...    forget-value!: ...              │
; global -->│ has-value?: ...               connect: ...                    │
;  env      │ get-value: ...                for-each-except: ─────────┐     │
;           │ make-connector: ─┐                                      │     │
;           └──────────────────┼──────────────────────────────────────┼─────┘
;                      ┌───────┘ Λ                                    │ Λ
;                      V         │                                    │ │
;           ┌─────────ꙨꙨ─────────┘                                    │ │
;           V                                                         │ │
; ⎛ params: -                                          ⎞              │ │
; ⎜ body: ((lambda (value informant constraints)       ⎟              │ │
; ⎜          (define (set-my-value newval setter) ...) ⎟              │ │
; ⎜          (define (forget-my-value retractor) ...)  ⎟              │ │
; ⎜          (define (connect new-constraint) ...)     ⎟  desugared   │ │
; ⎜          (define (me request) ...)                 ⎟  let syntax  │ │
; ⎜          me)                                       ⎟              │ │
; ⎜        false                                       ⎟              │ │
; ⎜        false                                       ⎟              │ │
; ⎝        '())                                        ⎠              │ │
;                                          ┌──────────────────────────┘ │
;                                          V                            │
;            ┌────────────────────────────ꙨꙨ────────────────────────────┘
;            V
;   params : exception, procedure, list
;   body: (define (loop items)
;           (cond ((null? items) 'done)
;                 ((eq? (car items) exception) (loop (cdr items)))
;                 (else (procedure (car items))
;                       (loop (cdr items)))))
;         (loop list)
;

(define a (make-connector))

;
;           ┌───────────────────────────────────────────────────────────────┐
;           │ inform-about-value: ...       set-value!: ...                 │
;           │ inform-about-no-value: ...    forget-value!: ...              │
; global -->│ has-value?: ...               connect: ...                    │
;  env      │ get-value: ...                for-each-except: ...            │
;           │ make-connector: ...                                           │
;           │ a: ─┐                                                         │
;           └─────┼─────────────────────────────────────────────────────────┘
;  ┌──────────────┘                             Λ
;  │                ┌───────────────────────────┴───────────────────────────┐
;  │                │ value: false               constraints: '()           │
;  │          E1 -->│ informant: false           me: ─────────────┐         │
;  │                │ forget-my-value: ─────┐    connect: ─┐      │         │
;  │                │ set-my-value: ─┐      │              │      │         │
;  │                └────────────────┼──────┼──────────────┼──────┼─────────┘
;  │ ┌───────────────────────────────┘ Λ    │ Λ            │ Λ    │ Λ
;  │ │ ┌───────────────────────────────┘    │ │            │ │    │ │
;  │ │ │                                    │ │            │ │    │ │
;  │ │ │    ┌───────────────────────────────┘ │            │ │    │ │
;  │ │ │    │ ┌───────────────────────────────┘            │ │    │ │
;  │ │ │    │ │                                            │ │    │ │
;  │ │ │    │ │    ┌───────────────────────────────────────┘ │    │ │
;  │ │ │    │ │    │ ┌───────────────────────────────────────┘    │ │
;  │ │ │    │ │    │ │                                            │ │
;  └─┼─┼────┼─┼────┼─┼───────────────────────┐┌───────────────────┘ │
;    │ │    │ │    │ │                       VV                     │
;    │ │    │ │    │ │ ┌─────────────────────ꙨꙨ─────────────────────┘
;    │ │    │ │    │ │ V
;    │ │    │ │    │ │ params: request
;    │ │    │ │    │ │ body: (cond ((eq? request 'has-value?)
;    │ │    │ │    │ │              (if informant true false))
;    │ │    │ │    │ │             ((eq? request 'value) value)
;    │ │    │ │    │ │             ((eq? request 'set-value!) set-my-value)
;    │ │    │ │    │ │             ((eq? request 'forget) forget-my-value)
;    │ │    │ │    │ │             ((eq? request 'connect) connect)
;    │ │    │ │    V │             (else (error "Unknown operation -- CONNECTOR"
;    │ │    │ │ ┌─ꙨꙨ─┘                          request))))
;    │ │    │ │ V
;    │ │    │ │ params: new-constraint
;    │ │    │ │ body: (if (not (memq new-constraint constraints))
;    │ │    │ │           (set! constraints
;    │ │    │ │                 (cons new-constraint constraints)))
;    │ │    │ │       (if (has-value? me)
;    │ │    V │           (inform-about-value new-constraint))
;    │ │ ┌─ꙨꙨ─┘       'done)
;    │ │ V
;    │ │ params: retractor
;    │ │ body: (if (eq? retractor informant)
;    │ │           (begin (set! informant false)
;    │ │                  (for-each-except retractor
;    │ │                                   inform-about-no-value
;    V │                                   constraints))
; ┌─ꙨꙨ─┘           'ignored))
; V
; params: newval, setter
; body: (cond ((not (has-value? me))
;              (set! value newval)
;              (set! informant setter)
;              (for-each-except setter
;                               inform-about-value
;                               constraints))
;             ((not (= value newval))
;              (error "Contradiction" (list value newval)))
;             (else 'ignored))
;

(define b (make-connector))

;
;           ┌───────────────────────────────────────────────────────────────┐
;           │ inform-about-value: ...       set-value!: ...                 │
;           │ inform-about-no-value: ...    forget-value!: ...              │
; global -->│ has-value?: ...               connect: ...                    │
;  env      │ get-value: ...                for-each-except: ...            │
;           │ make-connector: ...           b: ─┐                           │
;           │ a: ─┐                             │                           │
;           └─────┼─────────────────────────────┼───────────────────────────┘
;                 │            Λ                │            Λ
;                 │  E1        │                │  E2        │
;                 │ ┌──────────┴───────────┐    │ ┌──────────┴───────────┐
;                 │ │ value: false         │    │ │ value: false         │
;                 │ │ informant: false     │    │ │ informant: false     │
;                 │ │ constraints: '()     │    │ │ constraints: '()     │
;                 │ │ set-my-value: ...    │    │ │ set-my-value: ...    │
;                 │ │ forget-my-value: ... │    │ │ forget-my-value: ... │
;                 │ │ connect: ...         │    │ │ connect: ...         │
;                 │ │ me: ─┐               │    │ │ me: ─┐               │
;                 │ └──────┼───────────────┘    │ └──────┼───────────────┘
;                 │        │ Λ                  │        │ Λ
;                 └────┐┌──┘ │                  └────┐┌──┘ │
;                      VV    │                       VV    │
;                 ┌────ꙨꙨ────┘                  ┌────ꙨꙨ────┘
;                 V                             V
;                 params: request               params: request
;                 body: (cond ...)              body: (cond ...)
;

(set-value! a 10 'user)

;
;           ┌─────────────────────────────────────────────────────────────────┐
;           │ inform-about-value: ...       set-value!: ...                   │
;           │ inform-about-no-value: ...    forget-value!: ...                │
; global -->│ has-value?: ...               connect: ...                      │
;  env      │ get-value: ...                for-each-except: ...              │
;           │ make-connector: ...           b: ─┐                             │
;           │ a: ─┐                             │                             │
;           └─────┼─────────────────────────────┼─────────────────────────────┘
;             Λ   │            Λ                │            Λ              Λ
;             │   │ ┌──────────┴───────────┐    │ ┌──────────┴───────────┐  │
;             │   │ │ value: 10         E1 │    │ │ value: false      E2 │  │
;             │   │ │ informant: 'user     │    │ │ informant: false     │  │
;             │   │ │ constraints: '()     │    │ │ constraints: '()     │  │
;             │   │ │ set-my-value: ...    │    │ │ set-my-value: ...    │  │
;             │   │ │ forget-my-value: ... │    │ │ forget-my-value: ... │  │
;             │   │ │ connect: ...         │    │ │ connect: ...         │  │
;             │   │ │ me: ─┐               │    │ │ me: ─┐               │  │
;             │   │ └──────┼───────────────┘    │ └──────┼───────────────┘  │
;             │   │        │ Λ Λ Λ Λ            │        │ Λ                │
;             │   └────┐┌──┘ │ │ │ │            └────┐┌──┘ │                │
;             │        VV    │ │ │ └─────────┐       VV    │                │
;             │   ┌────ꙨꙨ────┘ │ └───────┐   │  ┌────ꙨꙨ────┘                │
;             │   │     Λ      └────┐    │   │  V                           │
;             │   V     └───────────┼────┼─┐ │  params: request             │
;             │   params: request   │    │ │ │  body: (cond ...)            │
;             │   body: (cond ...)  │    │ │ └─────────────┐        E7      │
;             │                     │    │ │     ┌─────────┴────────────┐   │
;             │    E4               │    │ │     │ request: 'has-value? │   │
;             │ ┌───────────────────┴──┐ │ │     └──────────────────────┘   │
;             │ │ request: 'set-value! │ │ │     (cond ...)                 │
;             │ └──────────────────────┘ │ │     ==> false                  │
;             │ (cond ...)               │ └───────────────────┐ E6         │
;             │ ==> set-my-value         │       ┌─────────────┼────┐       │
;        E3   │                          │       │ connector: ─┘    ├───────┘
;      ┌──────┴───────────┐              │       └──────────────────┘
;      │ connector: a     │              │       (connector 'has-value?)
;      │ new-value: 10    │              │       ==> false
;      │ informant: 'user │              │
;      └──────────────────┘              │
;      ((connector 'set-value!)          │
;       new-value                        │
;       informant)                       │
;      ==> ??                            │
;                                        │
;        E5   ┌──────────────────────────┘
;      ┌──────┴────────┐
;      │ newval: 10    │
;      │ setter: 'user │
;      └───────────────┘
;      (cond ((not (has-value? me))
;             (set! value newval)
;             (set! informant setter)
;           ⎧ (for-each-except setter               ⎫ next
;           ⎨                  inform-about-value   ⎬ expression
;           ⎩                  constraints))        ⎭ to evaluate
;            ((not (= value newval))
;             (error "Contradiction" (list value newval)))
;            (else 'ignored))
;      ==> ??
;
