(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

;
; (sum (lambda (x) x) 1 (lambda (x) (+ x 1)) 10)
; (iter 1 0)
; (iter 2 1)
; (iter 3 3)
; (iter 4 6)
; (iter 5 10)
; (iter 6 15)
; (iter 7 21)
; (iter 8 28)
; (iter 9 36)
; (iter 10 45)
; (iter 11 55)
; 55

(sum (lambda (x) x) 1 (lambda (x) (+ x 1)) 10)
