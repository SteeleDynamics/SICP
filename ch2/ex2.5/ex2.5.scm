;
; (a,b)   |-->  (2^a)*(3^b)
; #########################
; (0,0)         1
; (1,0)         2
; (1,1)         6
; (0,1)         3
; (2,0)         4
; (2,1)         12
; (2,2)         36
; (1,2)         18
; (0,2)         9
; ...           ...

(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (car p)
  (define (iter q acc)
    (if (> (remainder q 2) 0)
        acc
        (iter (/ q 2) (+ acc 1))))
  (iter p 0))

(define (cdr p)
  (define (iter q acc)
    (if (> (remainder q 3) 0)
        acc
        (iter (/ q 3) (+ acc 1))))
  (iter p 0))

(define p (cons 3 7))
p
(car p)
(cdr p)
