(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one (lambda (f) (lambda (x) (f x))))

(define two (lambda (f) (lambda (x) (f (f x)))))

(define (add n1 n2)
  (lambda (f) (lambda (x) ((n1 f) ((n2 f) x)))))

;
; one
; (add-1 zero)
; (add-1 (lambda (f) (lambda (x) x)))
; (lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x))))
; (lambda (f) (lambda (x) (f ((lambda (x) x) x))))
; (lambda (f) (lambda (x) (f x)))
;
; two
; (add-1 one)
; (add-1 (lambda (f) (lambda (x) (f x))))
; (lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) (f x))) f) x))))
; (lambda (f) (lambda (x) (f ((lambda (x) (f x)) x))))
; (lambda (f) (lambda (x) (f (f x))))
;
; (add zero one)
; (add (lambda (f) (lambda (x) x)) (lambda (f) (lambda (x) (f x)))
; (lambda (f) (lambda (x) (((lambda (f) (lambda (x) x)) f) (((lambda (f) (lambda (x) (f x))) f) x))))
; (lambda (f) (lambda (x) ((lambda (x) x) (((lambda (f) (lambda (x) (f x))) f) x))))
; (lambda (f) (lambda (x) ((lambda (x) x) ((lambda (x) (f x)) x))))
; (lambda (f) (lambda (x) ((lambda (x) x) (f x))))
; (lambda (f) (lambda (x) (f x)))
;
; (add one one)
; (add (lambda (f) (lambda (x) (f x))) (lambda (f) (lambda (x) (f x))))
; (lambda (f) (lambda (x) (((lambda (f) (lambda (x) (f x))) f) (((lambda (f) (lambda (x) (f x))) f) x))))
; (lambda (f) (lambda (x) ((lambda (x) (f x)) ((lambda (x) (f x)) x))))
; (lambda (f) (lambda (x) ((lambda (x) (f x)) (f x))))
; (lambda (f) (lambda (x) (f (f x))))
;
; (add one two)
; (add (lambda (f) (lambda (x) (f x))) (lambda (f) (lambda (x) (f (f x)))))
; (lambda (f) (lambda (x) (((lambda (f) (lambda (x) (f x))) f) (((lambda (f) (lambda (x) (f (f x)))) f) x))))
; (lambda (f) (lambda (x) ((lambda (x) (f x)) (((lambda (f) (lambda (x) (f (f x)))) f) x))))
; (lambda (f) (lambda (x) ((lambda (x) (f x)) ((lambda (x) (f (f x))) x))))
; (lambda (f) (lambda (x) (f (f (f x)))))
;
; (add two one)
; (add (lambda (f) (lambda (x) (f (f x)))) (lambda (f) (lambda (x) (f x))))
; (lambda (f) (lambda (x) (((lambda (f) (lambda (x) (f (f x)))) f) (((lambda (f) (lambda (x) (f x))) f) x))))
; (lambda (f) (lambda (x) ((lambda (x) (f (f x))) (((lambda (f) (lambda (x) (f x))) f) x))))
; (lambda (f) (lambda (x) ((lambda (x) (f (f x))) ((lambda (x) (f x)) x))))
; (lambda (f) (lambda (x) ((lambda (x) (f (f x))) (f x))))
; (lambda (f) (lambda (x) (f (f (f x)))))
;
; (add two two)
; (add (lambda (f) (lambda (x) (f (f x)))) (lambda (f) (lambda (x) (f (f x)))))
; (lambda (f) (lambda (x) (((lambda (f) (lambda (x) (f (f x)))) f) (((lambda (f) (lambda (x) (f (f x)))) f) x))))
; (lambda (f) (lambda (x) ((lambda (x) (f (f x))) ((lambda (x) (f (f x))) x))))
; (lambda (f) (lambda (x) ((lambda (x) (f (f x))) (f (f x)))))
; (lambda (f) (lambda (x) (f (f (f (f x))))))
