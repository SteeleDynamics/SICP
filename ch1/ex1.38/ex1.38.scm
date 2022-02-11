; iterative process
(define (cont-frac n d k)
  (define (iter i acc)
    (if (= i 0)
        acc
        (iter (- i 1) (/ (n i) (+ (d i) acc)))))
  (iter k 0))

; numerator of continued fraction expansion for e - 2
(define (n i) 1.0)

; denominator of continued fraction expansion for e - 2
(define (d i)
  (let ((q (floor (/ (- i 1) 3)))
        (r (remainder (- i 1) 3)))
       (cond ((= r 0) 1)
             ((= r 2) 1)
             (else (* (+ q 1) 2)))))

; unit test of (d i)
(map d '(1 2 3 4 5 6 7 8 9 10 11 12))

; e - 2 = 0.718281828
(cont-frac n d 16)
