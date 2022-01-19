(define (square x) (* x x))

(define (abs x)
  (if (< x 0)
      (- x)
      x))

(define (improve guess x)
  (/ (+ (/ x
           (square guess))
        (* 2 guess))
     3))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (good-enough? prev-guess curr-guess)
  (< (abs (- curr-guess prev-guess)) 0.001))

(define (cbrt-iter prev-guess curr-guess x)
  (if (good-enough? prev-guess curr-guess)
      curr-guess
      (cbrt-iter curr-guess
                 (improve curr-guess x)
                 x)))

(define (cbrt x)
  (cbrt-iter 0.0 1.0 x))

(cbrt 8); expected value 2

(cbrt 27); expected value 3

(cbrt 125); expected value 5

(cbrt -343); expected value -7

(cbrt -729); expected value -9

(cbrt -1331); expected value -11

