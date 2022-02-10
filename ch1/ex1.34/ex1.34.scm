(define (f g)
  (g 2))

(f square)

(f (lambda (z) (* z (+ z 1))))

;
; (f f)
; (f 2)
; (2 2)
; ==> The object 2 is not applicable.

(f f)
