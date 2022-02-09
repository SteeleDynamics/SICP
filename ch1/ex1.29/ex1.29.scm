(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (simp-int f a b n)
  (define (coeff k)
    (cond ((= k 0) 1)
          ((= k n) 1)
          ((odd? k) 4)
          (else 2)))
  (define h (/ (- b a) n))
  (define (y k)
    (f (+ a (* k h))))
  (sum (lambda (k) (* (/ h 3.0) (coeff k) (y k)))
       0
       (lambda (k) (+ k 1))
       n))

(define (cube x) (* x x x))

(integral cube 0 1 0.01)
(integral cube 0 1 0.001)

(simp-int cube 0 1 100)
(simp-int cube 0 1 1000)
