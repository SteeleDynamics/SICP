; Ackermann function
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(A 1 10)

(A 2 4)

(A 3 3)

(define (f n) (A 0 n))
;
; (f 3)
; (A 0 3)
; 6
; ==> f(n) = 2 * n

(define (g n) (A 1 n))
;
; (g 3)
; (A 1 3)
; (A 0 (A 1 2))
; (A 0 (A 0 (A 1 1)))
; (A 0 (A 0 2))
; (A 0 4)
; 8
; ==> g(n) = 2 ^ n

(define (h n) (A 2 n))
;
; (h 3)
; (A 2 3)
; (A 1 (A 2 2))
; (A 1 (h 2))
; (g (h 2))
; ==> h(n) = 2 ^ h(n-1)
