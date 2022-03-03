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

; make-frame procedure
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

; origin-frame procedure
(define (origin-frame frame)
  (car frame))

; edge1-frame procedure
(define (edge1-frame frame)
  (cadr frame))

; edge2-frame procedure
(define (edge2-frame frame)
  (caddr frame))

; frame-coord-map procedure
(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))

; make-segment procedure
(define (make-segment v1 v2)
  (list v1 v2))

; start-segment
(define (start-segment s)
  (car s))

; end-segment
(define (end-segment s)
  (cadr s))

; segments->painter procedure
(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))
