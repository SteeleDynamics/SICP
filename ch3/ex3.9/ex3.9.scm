; recursive factorial procedure
(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

; iterative factorial procedure
(define (factorial n)
  (fact-iter 1 1 n))

; fact-iter helper procedure
(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))

;
; Recursive factorial procedure:
; ##############################
;
;           ┌─────────────────┐
; global -->│ factorial: ─┐   │
;  env      └─────────────┼───┘
;                         │ ^
;                         V │
;                        ꙨꙨ─┘
;                        │
;                        V
;               parameters: n
;               body: (if (= n 1)
;                         1
;                         (* n (factorial (- n 1))))
; 
; Figure 1: Procedure object in the global frame.
;
;           ┌────────────────────────────────────────────────────────────────┐
; global -->│                                                                │
;  env      └────────────────────────────────────────────────────────────────┘
;             ^           ^           ^           ^           ^           ^   
;             │           │           │           │           │           │   
;           ┌─┴──┐      ┌─┴──┐      ┌─┴──┐      ┌─┴──┐      ┌─┴──┐      ┌─┴──┐
;      E1 ->│n: 6│ E2 ->│n: 5│ E3 ->│n: 4│ E4 ->│n: 3│ E5 ->│n: 2│ E6 ->│n: 1│
;           └────┘      └────┘      └────┘      └────┘      └────┘      └────┘
; 
; Figure 2: Environments created by evaluating (factorial 6) using the
;           procedures in figure 1. Each environment evaluates the body of
;           the function object factorial.
;
;
; Iterative factorial procedure:
; ##############################
;
;           ┌──────────────────────────────────────────┐
;           │ fact-iter: ──────────────────────────┐   │
; global -->│ factorial: ─┐                        │   │
;  env      └─────────────┼────────────────────────┼───┘
;                         │ ^                      │ ^          
;                         V │                      V │           
;                        ꙨꙨ─┘                     ꙨꙨ─┘           
;                        │                        │              
;                        V                        V              
;             parameters: n            parameters: product, counter, max-count           
;             body: (fact-iter 1 1 n)  body: (if (> counter max-count)
;                                                product
;                                                (fact-iter (* counter product)
;                                                           (+ counter 1)
;                                                           max-count))
; 
; Figure 3: Procedure objects in the global frame. 
;
;           ┌────────────────────────────────────────────────────────────────┐
; global -->│                                                                │
;  env      └────────────────────────────────────────────────────────────────┘
;             ^    ^        ^        ^        ^        ^        ^        ^
;             │    │        │        │        │        │        │        │
;           ┌─┴──┐ │        │        │        │        │        │        │
;      E1 ->│n: 6│ │        │        │        │        │        │        │
;           └────┘ │        │        │        │        │        │        │
;            ┌─────┴──────┐ │        │        │        │        │        │
;            │product: 1  │ │        │        │        │        │        │
;       E2 ->│counter: 1  │ │        │        │        │        │        │
;            │max-count: 6│ │        │        │        │        │        │
;            └────────────┘ │        │        │        │        │        │
;                     ┌─────┴──────┐ │        │        │        │        │
;                     │product: 1  │ │        │        │        │        │
;                E3 ->│counter: 2  │ │        │        │        │        │
;                     │max-count: 6│ │        │        │        │        │
;                     └────────────┘ │        │        │        │        │
;                              ┌─────┴──────┐ │        │        │        │
;                              │product: 2  │ │        │        │        │
;                         E4 ->│counter: 3  │ │        │        │        │
;                              │max-count: 6│ │        │        │        │
;                              └────────────┘ │        │        │        │
;                                       ┌─────┴──────┐ │        │        │
;                                       │product: 6  │ │        │        │
;                                  E5 ->│counter: 4  │ │        │        │
;                                       │max-count: 6│ │        │        │
;                                       └────────────┘ │        │        │
;                                                ┌─────┴──────┐ │        │
;                                                │product: 24 │ │        │
;                                           E6 ->│counter: 5  │ │        │
;                                                │max-count: 6│ │        │
;                                                └────────────┘ │        │
;                                                         ┌─────┴──────┐ │
;                                                         │product: 120│ │
;                                                    E7 ->│counter: 6  │ │
;                                                         │max-count: 6│ │
;                                                         └────────────┘ │
;                                                               ┌────────┴───┐
;                                                               │product: 720│
;                                                          E8 ->│counter: 7  │
;                                                               │max-count: 6│
;                                                               └────────────┘
; Figure 4: Environments created by evaluating (factorial 6) using the
;           procedures in figure 3. Environment E1 evaluates the body of
;           the function object factorial. The remaining environments evaluate
;           the body of the function object fact-iter.
;
