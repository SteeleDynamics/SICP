;
; Exercise 3.34:
; ##############
;
; Louis Reasoner wants to build a squarer, a constraint device with two
; terminals such that the value of connector b on the second terminal will
; always be the square of the value a on the first terminal. He proposes the
; following simple device made from a multiplier:
;
;     (define (squarer a b)
;       (multiplier a a b))
;
; There is a serious flaw in this idea. Explain.
;
;
; Answer:
; #######
;
; How the multiplier constraint is implemented it is possible to to arrive at
; a contradiction:
;
;   1 ]=> (define p (make-connector))
;   ;Value: p
;   
;   1 ]=> (define q (make-connector))
;   ;Value: q
;   
;   1 ]=> (begin (probe 'p p) (probe 'q q) 'ok)
;   ;Value: ok
;   
;   1 ]=> (begin (multiplier p p q) 'ok)
;   ;Value: ok
;   
;   1 ]=> (set-value! q 25 'user)
;   Probe: q = 25
;   ;Value: done
;   
;   1 ]=> (set-value! p 4 'user)
;   ;Contradiction (25 16)
;
; Flaws:
; ------
; 1. The constraint (multiplier a b p)  does not check if connectors a and b
;    have the same identity.
; 2. The procedure (square x) is not onto. This means the procedure (sqrt x) is
;    undefined for all x < 0.
; 3. The procedure (square x) is not one-to-one. Therefore, for all y > 0,
;    (define y (square x)) has 2 possible solutions: (sqrt y) and (- (sqrt y)).
;
