; iterative process
(define (cont-frac n d k)
  (define (iter i acc)
    (if (= i 0)
        acc
        (iter (- i 1) (/ (n i) (+ (d i) acc)))))
  (iter k 0))

; numerator of continued fraction expansion for tan x
(define (n x)
  (lambda (i)
    (if (< i 2)
        x
        (- (square x)))))

; denominator of continued fraction expansion for tan x
(define (d i) (- (* 2 i) 1))

; tan-cf is an approximation of a Lambert's formula
(define (tan-cf x k) (cont-frac (n x) d k))

; get approximation of pi radians
(define pi (acos -1))

; tan (pi / 3) = sqrt(3) = 1.732050808
(tan-cf (/ pi 3.0) 16)
