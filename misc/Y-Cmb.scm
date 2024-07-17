;; Y-Combinator procedure
(define Y
  (lambda (f)
    ((lambda (x) (f (lambda (a) ((x x) a))))
     (lambda (x) (f (lambda (a) ((x x) a)))))))

;; F procedure whose fixed-point solution is fact
(define F
  (lambda (g)
    (lambda (x)
      (if (< x 2)
          x
          (* x (g (- x 1)))))))

;; ((Y F) 6) ==> 720
((Y F) 6)
