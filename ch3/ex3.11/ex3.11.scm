(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)

;
;           ┌────────────────────┐
; global -->│ make-account: ─┐   │
;  env      └────────────────┼───┘
;                            │ Λ
;                            V │
;                ┌──────────ꙨꙨ─┘
;                V
;     params: balance
;     body: (define (withdraw amount) ...)
;           (define (deposit amount) ...)
;           (define (dispatch m) ...)
;           dispatch
;

(define acc (make-account 50))

;
;           ┌────────────────────────────────────────────┐
;           │ acc: ──────────────────────────────────────┼─────────────────┐
; global -->│ make-account: ─┐                           │                 │
;  env      └────────────────┼───────────────────────────┘                 │
;                            │ Λ                      Λ                    │
;                            V │                      │                    │
;                ┌──────────ꙨꙨ─┘            ┌─────────┴────────────────┐   │
;                V                     E1 ->│balance: 50               │   │
;     params: balance                       │dispatch: ──────────┐     │   │
;     body: (define (withdraw amount) ...)  │deposit: ──────┐    │     │   │
;           (define (deposit amount) ...)   │withdraw:─┐    │    │     │   │
;           (define (dispatch m) ...)       └──────────┼────┼────┼─────┘   │
;           dispatch                   ┌───────────────┘ Λ  │ Λ  │  Λ      │
;                                      V                 │  │ │  │  │      │
;           ┌─────────────────────────ꙨꙨ─────────────────┘  │ │  │  │      │
;           V                                               │ │  │  │      │
; params: amount                                            │ │  │  │      │
; body: (if (>= balance amount)                             │ │  │  │      │
;           (begin (set! balance (- balance amount))        │ │  │  │      │
;                  balance)            ┌────────────────────┘ │  │  │      │
;           "Insufficient funds")      V                      │  │  │      │
;           ┌─────────────────────────ꙨꙨ──────────────────────┘  │  │      │
;           V                                                    │  │      │
; params: amount                                                 │  │      │
; body: (set! balance (+ balance amount))                        │  │      │
;       balance                                                  │  │      │
;                                      ┌─────────────────────────┘  │      │
;                                      V                            │      │
;           ┌─────────────────────────ꙨꙨ────────────────────────────┘      │
;           V                          Λ                                   │
; params: m                            └───────────────────────────────────┘
; body: (cond ((eq? m 'withdraw) withdraw)
;            ((eq? m 'deposit) deposit)
;            (else (error "Unknown request -- MAKE-ACCOUNT"
;                         m)))
;

((acc 'deposit) 40)

;
;           ┌──────────────────────────────────┐
;           │ make-account: ...                │
; global -->│ acc: ─┐                          │
;  env      └───────┼──────────────────────────┘
;                   │                     Λ
;                   │                     │
;                   │       ┌─────────────┴────────────┐
;                   │       │balance: 50               │
;                   │  E1 ->│dispatch: ─────┐          │
;                   │       │deposit: ...   │          │
;                   │       │withdraw: ...  │          │
;                   │       └───────────────┼──────────┘
;                   └────────────┐ ┌────────┘  Λ    Λ
;                                V V           │    └───────┐
;                         ┌──────ꙨꙨ────────────┘      ┌─────┴─────┐
;                         V                      E2 ->│m: 'deposit│
;                      params: m                      └───────────┘
;                      body: ...                            Λ
;                                                     ┌─────┴─────┐
;                                                E3 ->│amount: 40 │
;                                                     └───────────┘
;                                             (set! balance (+ balance amount))
;                                             balance
;
;
;           ┌──────────────────────────────────┐
;           │ make-account: ...                │
; global -->│ acc: ─┐                          │
;  env      └───────┼──────────────────────────┘
;                   │                     Λ
;                   │                     │
;                   │       ┌─────────────┴────────────┐
;                   │       │balance: 90               │
;                   │  E1 ->│dispatch: ─────┐          │
;                   │       │deposit: ...   │          │
;                   │       │withdraw: ...  │          │
;                   │       └───────────────┼──────────┘
;                   └────────────┐ ┌────────┘  Λ
;                                V V           │
;                         ┌──────ꙨꙨ────────────┘
;                         V
;                      params: m
;                      body: ...
;

((acc 'withdraw) 60)

;
;           ┌──────────────────────────────────┐
;           │ make-account: ...                │
; global -->│ acc: ─┐                          │
;  env      └───────┼──────────────────────────┘
;                   │                     Λ
;                   │                     │
;                   │       ┌─────────────┴────────────┐
;                   │       │balance: 90               │
;                   │  E1 ->│dispatch: ─────┐          │
;                   │       │deposit: ...   │          │
;                   │       │withdraw: ...  │          │
;                   │       └───────────────┼──────────┘
;                   └────────────┐ ┌────────┘  Λ    Λ
;                                V V           │    └───────┐
;                         ┌──────ꙨꙨ────────────┘      ┌─────┴──────┐
;                         V                      E2 ->│m: 'withdraw│
;                      params: m                      └────────────┘
;                      body: ...                            Λ
;                                                     ┌─────┴─────┐
;                                                E3 ->│amount: 60 │
;                                                     └───────────┘
;                                   (if (>= balance amount)
;                                       (begin (set! balance (- balance amount))
;                                              balance)
;                                       "Insufficient funds"))
;
;
;           ┌──────────────────────────────────┐
;           │ make-account: ...                │
; global -->│ acc: ─┐                          │
;  env      └───────┼──────────────────────────┘
;                   │                     Λ
;                   │                     │
;                   │       ┌─────────────┴────────────┐
;                   │       │balance: 30               │
;                   │  E1 ->│dispatch: ─────┐          │
;                   │       │deposit: ...   │          │
;                   │       │withdraw: ...  │          │
;                   │       └───────────────┼──────────┘
;                   └────────────┐ ┌────────┘  Λ
;                                V V           │
;                         ┌──────ꙨꙨ────────────┘
;                         V
;                      params: m
;                      body: ...
;

(define acc2 (make-account 100))

;
;           ┌──────────────────────────────────────────────────────────────┐
;           │ make-account: ...                                            │
; global -->│ acc2: ──────────────────────────────┐                        │
;  env      │ acc: ─┐                             │                        │
;           └───────┼─────────────────────────────┼────────────────────────┘
;                   │                    Λ        │                    Λ
;                   │                    │        │                    │
;                   │      ┌─────────────┴─────┐  │      ┌─────────────┴─────┐
;                   │      │balance: 30        │  │      │balance: 100       │
;                   │ E1 ->│dispatch: ─────┐   │  │ E2 ->│dispatch: ─────┐   │
;                   │      │deposit: ...   │   │  │      │deposit: ...   │   │
;                   │      │withdraw: ...  │   │  │      │withdraw: ...  │   │
;                   │      └───────────────┼───┘  │      └───────────────┼───┘
;                   └───────────┐ ┌────────┘ Λ    └───────────┐ ┌────────┘ Λ
;                               V V          │                V V          │
;                        ┌──────ꙨꙨ───────────┘                ꙨꙨ───────────┘
;                        │  ┌─────────────────────────────────┘
;                        V  V
;                     params: m
;                     body: ...
;
; Q1: Where is the local state for acc kept?
; A1: Environment E1
;
; Q2: How are local states for the two accounts kept distinct?
; A2: New distinct environments E1 and E2 are created for each account.
;
; Q3: Which parts of the environment structure are shared between acc and acc2?
; A3: The code parts for dispatch, deposit, and withdraw are shared.
;
