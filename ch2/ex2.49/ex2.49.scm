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

; part (a)
(define outline-segments
  (list (make-segment (make-vect 0.0 0.0) (make-vect 1.0 0.0))
        (make-segment (make-vect 1.0 0.0) (make-vect 1.0 1.0))
        (make-segment (make-vect 1.0 1.0) (make-vect 0.0 1.0))
        (make-segment (make-vect 0.0 1.0) (make-vect 0.0 0.0))))
(define outline-painter (segments->painter outline-segments))

; part (b)
(define x-segments
  (list (make-segment (make-vect 0.0 0.0) (make-vect 1.0 1.0))
        (make-segment (make-vect 0.0 1.0) (make-vect 1.0 0.0))))
(define x-painter (segments->painter x-segments))

; part (c)
(define diamond-segments
  (list (make-segment (make-vect 0.5 0.0) (make-vect 1.0 0.5))
        (make-segment (make-vect 1.0 0.5) (make-vect 0.5 1.0))
        (make-segment (make-vect 0.5 1.0) (make-vect 0.0 0.5))
        (make-segment (make-vect 0.0 0.5) (make-vect 0.5 0.0))))
(define diamond-painter (segments->painter diamond-segments))

; part (d)
(define wave-segments
  (list (make-segment (make-vect 0.4 0.0) (make-vect 0.5 0.3))
        (make-segment (make-vect 0.5 0.3) (make-vect 0.6 0))
        (make-segment (make-vect 0.7 0) (make-vect 0.6 0.4))
        (make-segment (make-vect 0.6 0.4) (make-vect 1.0 0.2))
        (make-segment (make-vect 1.0 0.4) (make-vect 0.7 0.6))
        (make-segment (make-vect 0.7 0.6) (make-vect 0.6 0.6))
        (make-segment (make-vect 0.6 0.6) (make-vect 0.67 0.8))
        (make-segment (make-vect 0.67 0.8) (make-vect 0.6 1.0))
        (make-segment (make-vect 0.4 1.0) (make-vect 0.33 0.8))
        (make-segment (make-vect 0.33 0.8) (make-vect 0.4 0.6))
        (make-segment (make-vect 0.4 0.6) (make-vect 0.3 0.6))
        (make-segment (make-vect 0.3 0.6) (make-vect 0.2 0.55))
        (make-segment (make-vect 0.2 0.55) (make-vect 0.0 0.8))
        (make-segment (make-vect 0.0 0.6) (make-vect 0.2 0.35))
        (make-segment (make-vect 0.2 0.35) (make-vect 0.3 0.55))
        (make-segment (make-vect 0.3 0.55) (make-vect 0.35 0.45))
        (make-segment (make-vect 0.35 0.45) (make-vect 0.0 0.4))))
(define wave-painter (segments->painter wave-segments))
