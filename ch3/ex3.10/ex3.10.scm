; make-withdraw procedure (problem statement)
(define (make-withdraw initial-amount)
  (let ((balance initial-amount))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))

;
; Syntactic Sugar:
; ################
; (let ((⟨var⟩ ⟨exp⟩)) ⟨body⟩) <==> ((lambda (⟨var⟩) ⟨body⟩) ⟨exp⟩)
;

; desugared make-withdraw procedure
(define (make-withdraw initial-amount)
  ((lambda (balance)
    (lambda (amount)
      (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")))
   initial-amount))

;
;           ┌────────────────────┐
; global -->│ make-witdraw: ─┐   │
;  env      └────────────────┼───┘
;                            │ Λ
;                            V │
;                           ꙨꙨ─┘
;                           │
;                           V
;                  params: initial-amount
;                  body: ((lambda (balance)
;                          (lambda (amount)
;                            (if (>= balance amount)
;                              (begin (set! balance (- balance amount))
;                                     balance)
;                              "Insufficient funds")))
;                         initial-amount)
;

(define W1 (make-withdraw 100))

;
;           ┌──────────────────────────────────────────────┐
;           │ make-withdraw: ──────────────────────────┐   │
; global -->│                                          │   │
;  env      └──────────────────────────────────────────┼───┘
;                Λ                                     │ Λ
;                │                                     │ │
;      ┌─────────┴─────────┐                           V │
; E1 ->│inital-amount: 100 │                          ꙨꙨ─┘
;      └───────────────────┘                          │
;     ((lambda (balance)                              V
;       (lambda (amount)                              params: initial-amount
;         (if (>= balance amount)                     body: ...
;           (begin (set! balance (- balance amount))
;                  balance)
;         "Insufficient funds")))
;       initial-amount)
;
;
;           ┌───────────────────────────────────────────────────┐
;           │ make-withdraw: ───────────────────────────────┐   │
; global -->│ W1: ─┐                                        │   │
;  env      └──────┼────────────────────────────────────────┼───┘
;                  │                Λ                       │ Λ
;                  │                │                       │ │
;                  │      ┌─────────┴─────────┐             V │
;                  │ E1 ->│inital-amount: 100 │            ꙨꙨ─┘
;                  │      └───────────────────┘            │
;                  │                Λ                      V
;                  │                │                   params: initial-amount
;                  │         ┌──────┴──────┐            body: ...
;                  │    E2 ->│balance: 100 │
;                  │         └─────────────┘
;                  │                Λ
;                  V                │
;                 ꙨꙨ────────────────┘
;                 │
;                 │
;                 V
;           params: amount
;           body: (if (>= balance amount)
;                     (begin (set! balance (- balance amount))
;                            balance)
;                     "Insufficient funds")
;

(W1 50)

;
;           ┌───────────────────────────────────────────────────┐
;           │ make-withdraw: ───────────────────────────────┐   │
; global -->│ W1: ─┐                                        │   │
;  env      └──────┼────────────────────────────────────────┼───┘
;                  │                Λ                       │ Λ
;                  │                │                       │ │
;                  │      ┌─────────┴─────────┐             V │
;                  │ E1 ->│inital-amount: 100 │            ꙨꙨ─┘
;                  │      └───────────────────┘            │
;                  │                Λ                      V
;                  │                │                   params: initial-amount
;                  │         ┌──────┴──────┐            body: ...
;                  │    E2 ->│balance: 100 │
;                  │         └─────────────┘
;                  │                Λ   Λ
;                  V                │   └─────────────────┐
;                 ꙨꙨ────────────────┘               ┌─────┴─────┐
;                 │                            E3 ->│amount: 50 │
;                 V                                 └───────────┘
;           params: amount                  (if (>= balance amount)
;           body: ...                           (begin (set! balance
;                                                            (- balance amount))
;                                                       balance)
;                                               "Insufficient funds")
;
;
;           ┌───────────────────────────────────────────────────┐
;           │ make-withdraw: ───────────────────────────────┐   │
; global -->│ W1: ─┐                                        │   │
;  env      └──────┼────────────────────────────────────────┼───┘
;                  │                Λ                       │ Λ
;                  │                │                       │ │
;                  │      ┌─────────┴─────────┐             V │
;                  │ E1 ->│inital-amount: 100 │            ꙨꙨ─┘
;                  │      └───────────────────┘            │
;                  │                Λ                      V
;                  │                │                   params: initial-amount
;                  │         ┌──────┴──────┐            body: ...
;                  │    E2 ->│balance: 50  │
;                  │         └─────────────┘
;                  │                Λ
;                  V                │
;                 ꙨꙨ────────────────┘
;                 │
;                 V
;           params: amount
;           body: ...
;

(define W2 (make-withdraw 100))

;
;           ┌────────────────────────────────────────────────────────────────┐
;           │ make-withdraw: ...                                             │
;           │ W2: ───────────────────────────────┐                           │
; global -->│ W1: ─┐                             │                           │
;  env      └──────┼─────────────────────────────┼───────────────────────────┘
;                  │                Λ            │                Λ
;                  │                │            │                │
;                  │      ┌─────────┴─────────┐  │      ┌─────────┴─────────┐
;                  │ E1 ->│inital-amount: 100 │  │ E4 ->│inital-amount: 100 │
;                  │      └───────────────────┘  │      └───────────────────┘
;                  │                Λ            │                Λ
;                  │                │            │                │
;                  │         ┌──────┴──────┐     │         ┌──────┴──────┐
;                  │    E2 ->│balance: 50  │     │    E5 ->│balance: 100 │
;                  │         └─────────────┘     │         └─────────────┘
;                  │                Λ            │                Λ
;                  V                │            V                │
;                 ꙨꙨ────────────────┘           ꙨꙨ────────────────┘
;                 │  ┌──────────────────────────┘
;                 V  V
;           params: amount
;           body: ...
;
;
; The main difference between the environment stuctures created by the
; make-withdraw procedure defined at the beginning of §3.2.3 and the
; make-withdraw procedure defined in this exercise is the creation of the
; additional enviroments E1 and E4. The let-expression creates another frame
; whose sole purpose is to bind the value of initial-amount to balance. As we
; can see, the let-expression is effectively redundant.
;
