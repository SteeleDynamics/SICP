;
; Exercise 3.18
; #############
;
; Write a procedure that examines a list and determines whether it contains a
; cycle, that is, whether a program that tried to find the end of the list by
; taking successive cdrs would go into an infinite loop. Exercise 3.13
; constructed such lists.
'()

; memq procedure
(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

; cycle? procedure
(define (cycle? x)
  (define (safe-cdr elt)
    (if (pair? elt)
        (cdr elt)
        '()))
  (define visited '())
  (define (iter u)
    (cond ((null? u) false)
          ((memq u visited) true)
          (else (set! visited (cons u visited))
                (iter (safe-cdr u)))))
  (iter x))

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
