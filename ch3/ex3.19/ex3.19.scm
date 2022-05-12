; cycle? predicate procedure (Floyd's cycle-finding algorithm)
(define (cycle? x)
  (define (safe-cdr e)
    (if (pair? e)
        (cdr e)
        '()))
  (define (iter a b)
    (cond ((null? a) false)
          ((null? b) false)
          ((eq? a b) true)
          (else (iter (safe-cdr a) (safe-cdr (safe-cdr b))))))
  (iter (safe-cdr x) (safe-cdr (safe-cdr x))))

;
;      ┌─┐    ┌─┐    ┌─┐    ┌─┐    ┌─┐    ┌─┐
;      │a│    │b│    │c│    │e│    │f│    │g│
;      └─┘    └─┘    └─┘    └─┘    └─┘    └─┘
;       Λ      Λ      Λ      Λ      Λ      Λ
;      ┌┼┬─┐  ┌┼┬─┐  ┌┼┬─┐  ┌┼┬─┐  ┌┼┬─┐  ┌┼┬─┐
;  w ─>│•│•┼─>│•│•┼─>│•│•┼─>│•│•┼─>│•│•┼─>│•│╱│
;      └─┴─┘  └─┴─┘  └─┴─┘  └─┴─┘  └─┴─┘  └─┴─┘
;

(define w (list 'a 'b 'c 'e 'f 'g))
(cycle? w)

;
;      ┌─┐    ┌─┐    ┌─┐    ┌─┐    ┌─┐    ┌─┐
;      │a│    │b│    │c│    │e│    │f│    │g│
;      └─┘    └─┘    └─┘    └─┘    └─┘    └─┘
;       Λ      Λ      Λ      Λ      Λ      Λ
;      ┌┼┬─┐  ┌┼┬─┐  ┌┼┬─┐  ┌┼┬─┐  ┌┼┬─┐  ┌┼┬─┐
;  x ─>│•│•┼─>│•│•┼─>│•│•┼─>│•│•┼─>│•│•┼─>│•│•┼┐
;      └─┴─┘  └─┴─┘  └─┴─┘  └─┴─┘  └─┴─┘  └─┴─┘│
;       Λ                                      │
;       └──────────────────────────────────────┘

(define x (list 'a 'b 'c 'e 'f 'g))
(set-cdr! (last-pair x) x)
(cycle? x)

;
;      ┌─┐    ┌─┐    ┌─┐    ┌─┐    ┌─┐    ┌─┐
;      │a│    │b│    │c│    │e│    │f│    │g│
;      └─┘    └─┘    └─┘    └─┘    └─┘    └─┘
;       Λ      Λ      Λ      Λ      Λ      Λ
;      ┌┼┬─┐  ┌┼┬─┐  ┌┼┬─┐  ┌┼┬─┐  ┌┼┬─┐  ┌┼┬─┐
;  y ─>│•│•┼─>│•│•┼─>│•│•┼─>│•│•┼─>│•│•┼─>│•│•┼┐
;      └─┴─┘  └─┴─┘  └─┴─┘  └─┴─┘  └─┴─┘  └─┴─┘│
;                     Λ                        │
;                     └────────────────────────┘

(define y (list 'a 'b 'c 'e 'f 'g))
(set-cdr! (last-pair y) (cddr y))
(cycle? y)

;
;      ┌─┐    ┌─┐    ┌─┐    ┌─┐    ┌─┐    ┌─┐
;      │a│    │b│    │c│    │e│    │f│    │g│
;      └─┘    └─┘    └─┘    └─┘    └─┘    └─┘
;       Λ      Λ      Λ      Λ      Λ      Λ
;      ┌┼┬─┐  ┌┼┬─┐  ┌┼┬─┐  ┌┼┬─┐  ┌┼┬─┐  ┌┼┬─┐
;  z ─>│•│•┼─>│•│•┼─>│•│•┼─>│•│•┼─>│•│•┼─>│•│•┼┐
;      └─┴─┘  └─┴─┘  └─┴─┘  └─┴─┘  └─┴─┘  └─┴─┘│
;                                          Λ   │
;                                          └───┘

(define z (list 'a 'b 'c 'e 'f 'g))
(set-cdr! (last-pair z) (last-pair z))
(cycle? z)
