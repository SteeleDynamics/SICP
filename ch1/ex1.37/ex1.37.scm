;
; k-Term Finite Continued Fraction
;
;       N1
; --------------
;         N2
; D1 + ---------
;     ...    Nk
;         + ----
;            Dk

()

; recursive process
(define (cont-frac-rec n d k)
  (define (rec i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (rec (+ i 1))))))
  (rec 1))

; iterative process
(define (cont-frac-iter n d k)
  (define (iter i acc)
    (if (= i 0)
        acc
        (iter (- i 1) (/ (n i) (+ (d i) acc)))))
  (iter k 0))

; 1 / phi = 0.618033989
(cont-frac-rec (lambda (x) 1.0)
               (lambda (x) 1.0)
               11)

; 1 / phi = 0.618033989
(cont-frac-iter (lambda (x) 1.0)
                (lambda (x) 1.0)
                11)
