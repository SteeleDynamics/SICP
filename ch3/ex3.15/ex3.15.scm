(define x (list 'a 'b))
(define z1 (cons x x))

;
;      ┌─┬─┐
; z1 ─>│•│•│
;      └┼┴┼┘
;       V V
;      ┌─┬─┐  ┌─┬─┐
;  x ─>│•│•┼─>│•│╱│
;      └┼┴─┘  └┼┴─┘
;       V      V
;      ┌─┐    ┌─┐
;      │a│    │b│
;      └─┘    └─┘
;

(define z2 (cons (list 'a 'b) (list 'a 'b)))

;
;      ┌─┬─┐  ┌─┬─┐  ┌─┬─┐
; z2 ─>│•│•┼─>│•│•┼─>│•│╱│
;      └┼┴─┘  └┼┴─┘  └┼┴─┘
;       │      V      V
;       │     ┌─┐    ┌─┐
;       │     │a│    │b│
;       │     └─┘    └─┘
;       │      Λ      Λ
;       │     ┌┼┬─┐  ┌┼┬─┐
;       └────>│•│•┼─>│•│╱│
;             └─┴─┘  └─┴─┘
;

(define (set-to-wow! x)
  (set-car! (car x) 'wow)
  x)

z1
(set-to-wow! z1)

;
;       ┌─┬─┐
;  z1 ─>│•│•│
;       └┼┴┼┘
;        V V
;       ┌─┬─┐  ┌─┬─┐
;   x ─>│•│•┼─>│•│╱│
;       └┼┴─┘  └┼┴─┘
;   ┌────┘      │
;   V           V
; ┌───┐ ┌─┐    ┌─┐
; │wow│ │a│    │b│
; └───┘ └─┘    └─┘
;

z2
(set-to-wow! z2)

;
;      ┌─┬─┐  ┌─┬─┐  ┌─┬─┐
; z2 ─>│•│•┼─>│•│•┼─>│•│╱│
;      └┼┴─┘  └┼┴─┘  └┼┴─┘
;       │      V      V
;       │     ┌─┐    ┌─┐
;       │     │a│    │b│
;       │     └─┘    └─┘
;       │             Λ
;       │     ┌─┬─┐  ┌┼┬─┐
;       └────>│•│•┼─>│•│╱│
;             └┼┴─┘  └─┴─┘
;              V
;            ┌───┐
;            │wow│
;            └───┘
;
