; make-vect procedure
(define (make-vect x y)
  (list x y))

; xcor-vect procedure
(define (xcor-vect v)
  (car v))

; ycor-vect procedure
(define (ycor-vect v)
  (cadr v))

; add-vect procedure
(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v2) (ycor-vect v2))))

; sub-vect procedure
(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
             (- (ycor-vect v1) (ycor-vect v2))))

; scale-vect procedure
(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))
