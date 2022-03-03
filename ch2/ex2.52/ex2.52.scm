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

; transform-painter procedure
(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
         (make-frame new-origin
                     (sub-vect (m corner1) new-origin)
                     (sub-vect (m corner2) new-origin)))))))

; identity procedure
(define (identity painter) painter)

; flip-vert procedure
(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

; rotate90 procedure (counterclockwise)
(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

; squash-inwards procedure
(define (squash-inwards painter)
  (transform-painter painter
                     (make-vect 0.0 0.0)
                     (make-vect 0.65 0.35)
                     (make-vect 0.35 0.65)))

; beside procedure
(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              split-point
                              (make-vect 0.0 1.0)))
          (paint-right
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.0)
                              (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

; flip-horiz procedure
(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

; rotate180 procedure (counterclockwise)
(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))

; rotate270 procedure (counterclockwise)
(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

; below procedure
(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-bottom
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              (make-vect 1.0 0.0)
                              split-point))
          (paint-top
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.5)
                              (make-vect 0.0 1.0))))
      (lambda (frame)
        (paint-bottom frame)
        (paint-top frame)))))

; right-split procedure
(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

; up-split procedure
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

; corner-split procedure, one copy of up-split and right-split
(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1)))
            (corner (corner-split painter (- n 1))))
        (beside (below painter up)
                (below right corner))))))

; square-limit1 procedure
(define (square-limit1 painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

; square-of-four procedure
(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

; square-limit2 procedure, make Mr. Rogers look outward from ea corner of square
(define (square-limit2 painter n)
  (let ((combine4 (square-of-four flip-vert rotate180
                                  identity flip-horiz)))
    (combine4 (corner-split painter n))))

; split HOF procedure
(define (split proc1 proc2)
  (define (rec painter n)
    (if (= n 0)
        painter
        (let ((smaller (rec painter (- n 1))))
          (proc1 painter (proc2 smaller smaller)))))
  rec)

; wave-segments and wave-painter, added smile
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
        (make-segment (make-vect 0.35 0.45) (make-vect 0.0 0.4))
        (make-segment (make-vect 0.45 0.7) (make-vect 0.5 0.65))
        (make-segment (make-vect 0.5 0.65) (make-vect 0.55 0.7))))
(define wave-painter (segments->painter wave-segments))
