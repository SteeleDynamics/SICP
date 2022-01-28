(define (f-rec n)
  (if (< n 3)
      n
      (+ (f-rec (- n 1))
         (* 2 (f-rec (- n 2)))
         (* 3 (f-rec (- n 3))))))

(define (f-iter n)
  (f-iter-impl 0 1 2 n))

(define (f-iter-impl a b c count)
  (if (= count 0)
      b
      (f-iter-impl (+ a
                      (* 2 b)
                      (* 3 c))
                   a
                   b
                   (- count 1))))
