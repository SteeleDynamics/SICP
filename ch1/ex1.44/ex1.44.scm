(define (compose f g)
  (lambda (x) (f (g x))))

(define (id x) x)

(define (repeated f n)
  (define (iter f i acc)
    (if (> i n)
      acc
      (iter f (+ i 1) (compose f acc))))
  (iter f 1 id))

(define dx 0.000001)

(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x dx))
          (f x)
          (f (+ x dx)))
       3.0)))

(define (n-fold-smooth f n)
  (repeated (smooth f) n))

((n-fold-smooth cos 4) 1)
