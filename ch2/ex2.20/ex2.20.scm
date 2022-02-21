;
; Exercise introducing dotted-tail notation in Lisp/Scheme

;
; same-parity : 'a list -> 'a list
; REQUIRES: xs is a non-empty list
; ENSURES: (same-parity x_1 x_2 .... x_n) ==> (x_1 y_1 y_2 ... y_k) such that
;          for all y_k in (x_2 x_3 ... x_n), x_1 and y_k are all even or odd
(define (same-parity . xs)
  (define (make-pred f v) (lambda (x) (= (f x) (f v))))
  (define (parity x) (remainder x 2))
  (define (filter pred? xs)
    (cond ((null? xs) xs)
          ((pred? (car xs)) (cons (car xs) (filter pred? (cdr xs))))
          (else (filter pred? (cdr xs)))))
    (filter (make-pred parity (car xs)) xs))

; same-parity unit tests
(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)
