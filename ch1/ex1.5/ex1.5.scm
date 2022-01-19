(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

; applicative-order (eager) evaluation ==> infinite loop
; normal-order (lazy) evaluation ==> value 0
(test 0 (p))
