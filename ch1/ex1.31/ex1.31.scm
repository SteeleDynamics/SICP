(define (product-rec term a next b)
  (if (> a b)
      1
      (* (term a)
         (product-rec term (next a) next b))))

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

(define (factorial n)
  (product-rec (lambda (x) x) 1 (lambda (x) (+ x 1)) n))

(define (f i) (* 2 (floor (/ i 2))))

(define (numer i) (f (+ i 3)))

(define (denom i) (+ (f (+ i 2)) 1))

(define (term i) (/ (numer i) (denom i)))

(define (incr i) (+ i 1))

(* 4.0 (product-iter term 0 incr 999))
