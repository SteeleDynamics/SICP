; last-pair procedure
(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

; make-cycle procedure
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c)))

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
; (last-pair z) results in an infinite loop
;

(last-pair z)
