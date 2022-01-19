(define (square x) (* x x))

(define (abs x)
  (if (< x 0)
      (- x)
      x))

(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(sqrt 2); expected value 1.414213562

(sqrt 0.000244140625); expected value 0.015625

; loops forever
;(sqrt 251728825683549488150424261); expected value 15865964379247.4678601874

(define (good-enough? prev-guess curr-guess)
  (< (abs (- curr-guess prev-guess)) 0.001))

(define (sqrt-iter prev-guess curr-guess x)
  (if (good-enough? prev-guess curr-guess)
      curr-guess
      (sqrt-iter curr-guess
                 (improve curr-guess x)
                 x)))

(define (sqrt x)
  (sqrt-iter 0.0 1.0 x))

(sqrt 2); expected value 1.414213562

(sqrt 0.000244140625); expected value 0.015625

(sqrt 251728825683549488150424261); expected value 15865964379247.4678601874

