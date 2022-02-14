(define (iterative-improve good-enough? improve)
  (define (iter guess)
    (if (good-enough? guess)
        guess
        (iter (improve guess))))
  iter)

(define (improve x)
  (lambda (guess)
    (average guess (/ x guess))))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? x)
  (lambda (guess)
    (< (abs (- (square guess) x)) 0.001)))

(define (sqrt x)
  ((iterative-improve (good-enough? x) (improve x)) 1.0))

(sqrt 2)

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? x)
    (< (abs (- (f x) x)) tolerance))
  (define (try guess)
    ((iterative-improve close-enough? f) guess))
  (try first-guess))

(fixed-point cos 1.0)
