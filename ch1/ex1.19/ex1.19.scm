(define (even? n)
  (= (remainder n 2) 0))

(define (fib n)
  (fib-iter 1 0 0 1 n))

;
; T_pq (a,b) = (bq + aq + ap, bp + aq)
;
; (T_pq o T_pq) (a,b)          = T (bq + aq + ap, bp + aq)
;                              = ((bp + aq)q + (bq + aq + ap)q + (bq + aq + ap)p,
;                                 (bp + aq)p + (bq + aq + ap)q)
;                              = (bpq + aqq + bqq + aqq + apq + bpq + apq + app,
;                                 bpp + apq + bqq + aqq + apq)
;                              = (app + 2apq + 2aqq + 2bpq + bqq,
;                                 2apq + aqq + bpp + bqq
; (bq' + aq' + ap', bp' + aq') = (b(2pq + qq) + a(2pq + qq) + a(pp + qq),
;                                 b(pp + qq) + a(2pq + qq))
; --> p' = pp + qq
; --> q' = 2pq + qq

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (* p p) (* q q))
                   (+ (* 2 p q) (* q q))
                   (/ count 2)))
        (else
         (fib-iter (+ (* b q) (* a q) (* a p))
                   (+ (* b p) (* a q))
                   p
                   q
                   (- count 1)))))

;
; (fib 73) --> 806515533049393

(fib 73)
