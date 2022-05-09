; count-pairs procedure
(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(define x (list 'a 'b 'c))
(count-pairs x)

;
;      ┌─┬─┐  ┌─┬─┐  ┌─┬─┐
;  x ─>│•│•┼─>│•│•┼─>│•│╱│
;      └┼┴─┘  └┼┴─┘  └┼┴─┘
;       V      V      V
;      ┌─┐    ┌─┐    ┌─┐
;      │a│    │b│    │c│
;      └─┘    └─┘    └─┘
;

(define x (list 'a 'b))
(define y (cons x (cdr x)))
(count-pairs y)

;
;      ┌─┬─┐
;  y ─>│•│•┼───┐
;      └┼┴─┘   │
;       V      V
;      ┌─┬─┐  ┌─┬─┐
;  x ─>│•│•┼─>│•│╱│
;      └┼┴─┘  └┼┴─┘
;       V      V
;      ┌─┐    ┌─┐
;      │a│    │b│
;      └─┘    └─┘
;

(define x (list 'a))
(define y (cons x x))
(define z (cons y y))
(count-pairs z)

;
;      ┌─┬─┐
;  z ─>│•│•│
;      └┼┴┼┘
;       V V
;      ┌─┬─┐
;  y ─>│•│•│
;      └┼┴┼┘
;       V V
;      ┌─┬─┐
;  x ─>│•│╱│
;      └┼┴─┘
;       V
;      ┌─┐
;      │a│
;      └─┘
;

(define x (list 'a 'b 'c))
(set-cdr! (last-pair x) x)
(count-pairs x)

;
;      ┌─┐    ┌─┐    ┌─┐  
;      │a│    │b│    │c│  
;      └─┘    └─┘    └─┘  
;       Λ      Λ      Λ   
;      ┌┼┬─┐  ┌┼┬─┐  ┌┼┬─┐
;  x ─>│•│•┼─>│•│•┼─>│•│•┼┐
;      └─┴─┘  └─┴─┘  └─┴─┘│
;       Λ                 │
;       └─────────────────┘
;
