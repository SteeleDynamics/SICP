(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (newline)
    (display guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define average (lambda (x y) (/ (+ x y) 2)))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define f (lambda (x) (/ (log 1000) (log x))))

(define g (average-damp f))

(fixed-point f 2.0)
(fixed-point g 2.0)
