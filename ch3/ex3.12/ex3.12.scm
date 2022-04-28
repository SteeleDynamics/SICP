; append procedure
(define (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))

; append! mutator procedure
(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

; last-pair procedure
(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

; define list x (see box-and-pointer diagram below)
(define x (list 'a 'b))

;
;      ┌─┐        ┌─┐
;      │a│        │b│
;      └─┘        └─┘
;       Λ          Λ
;      ┌┼┬─┐      ┌┼┬─┐
;  x ─>│•│•┼─────>│•│╱│
;      └─┴─┘      └─┴─┘
;

; define list y (see box-and-pointer diagram below)
(define y (list 'c 'd))

;
;      ┌─┐        ┌─┐        ┌─┐        ┌─┐
;      │a│        │b│        │c│        │d│
;      └─┘        └─┘        └─┘        └─┘
;       Λ          Λ          Λ          Λ
;      ┌┼┬─┐      ┌┼┬─┐      ┌┼┬─┐      ┌┼┬─┐
;  x ─>│•│•┼─────>│•│╱│  y ─>│•│•┼─────>│•│╱│
;      └─┴─┘      └─┴─┘      └─┴─┘      └─┴─┘
;

; define list z (see box-and-pointer diagram below)
(define z (append x y))

;
;      ┌─┬─┐      ┌─┬─┐      ┌─┬─┐      ┌─┬─┐
;  z ─>│•│•┼─────>│•│•┼─────>│•│•┼─────>│•│╱│
;      └┼┴─┘      └┼┴─┘      └┼┴─┘      └┼┴─┘
;       V          V          V          V
;      ┌─┐        ┌─┐        ┌─┐        ┌─┐
;      │a│        │b│        │c│        │d│
;      └─┘        └─┘        └─┘        └─┘
;       Λ          Λ          Λ          Λ
;      ┌┼┬─┐      ┌┼┬─┐      ┌┼┬─┐      ┌┼┬─┐
;  x ─>│•│•┼─────>│•│╱│  y ─>│•│•┼─────>│•│╱│
;      └─┴─┘      └─┴─┘      └─┴─┘      └─┴─┘
;

; z ==> (a b c d)
z

; (cdr x) ==> (b)
(cdr x)

; define list w by mutating x (see box-and-pointer diagram below)
(define w (append! x y))

;
;      ┌─┬─┐      ┌─┬─┐      ┌─┬─┐      ┌─┬─┐
;  z ─>│•│•┼─────>│•│•┼─────>│•│•┼─────>│•│╱│
;      └┼┴─┘      └┼┴─┘      └┼┴─┘      └┼┴─┘
;       V          V          V          V
;      ┌─┐        ┌─┐        ┌─┐        ┌─┐
;      │a│        │b│        │c│        │d│
;      └─┘        └─┘        └─┘        └─┘
;       Λ          Λ          Λ          Λ
;      ┌┼┬─┐      ┌┼┬─┐      ┌┼┬─┐      ┌┼┬─┐
;  x ─>│•│•┼─────>│•│•┼─────>│•│•┼─────>│•│╱│
;      └─┴─┘      └─┴─┘      └─┴─┘      └─┴─┘
;       Λ                     Λ
;       │                     │
;       w                     y

; w ==> (a b c d)
w

; (cdr x) ==> (b c d)
(cdr x)
