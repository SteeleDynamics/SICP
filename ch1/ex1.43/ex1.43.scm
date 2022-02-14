(define (compose f g)
  (lambda (x) (f (g x))))

(define (id x) x)

(define (repeated f n)
  (define (iter f i acc)
    (if (> i n)
      acc
      (iter f (+ i 1) (compose f acc))))
  (iter f 1 id))

((repeated square 2) 5)
