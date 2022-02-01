(define (even? n)
  (= (remainder n 2) 0))

(define (fast-expt-iter a b n)
  (cond ((= n 0) a)
        ((even? n) (fast-expt-iter a (* b b) (/ n 2)))
        (else (fast-expt-iter (* a b) b (- n 1)))))

;
; (fast-expt-iter 1 3 13)
; (fast-expt-iter 3 3 12)
; (fast-expt-iter 3 9 6)
; (fast-expt-iter 3 81 3)
; (fast-expt-iter 243 81 2)
; (fast-expt-iter 243 6561 1)
; (fast-expt-iter 1594323 6561 0)
; 1594323

(fast-expt-iter 1 3 13)
