(define (even? n)
  (= (remainder n 2) 0))

(define (double a) (* a 2))

(define (halve a) (/ a 2))

;
; invariant quantity: acc + (a * b)
(define (fast-mult-iter a b acc)
  (cond ((= b 0) acc)
         ((even? b) (fast-mult-iter (double a) (halve b) acc))
         (else (fast-mult-iter a (- b 1) (+ acc a)))))

;
; (fast-mult-iter 3 7 0)
; (fast-mult-iter 3 6 3)
; (fast-mult-iter 6 3 3)
; (fast-mult-iter 6 2 9)
; (fast-mult-iter 12 1 9)
; (fast-mult-iter 12 0 21)
; 21

(fast-mult-iter 3 7 0)
