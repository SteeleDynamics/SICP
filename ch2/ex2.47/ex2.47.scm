; make-frame1 procedure
(define (make-frame1 origin edge1 edge2)
  (list origin edge1 edge2))

; origin-frame1 procedure
(define (origin-frame1 frame)
  (car frame))

; edge1-frame1 procedure
(define (edge1-frame1 frame)
  (cadr frame))

; edge2-frame1 procedure
(define (edge2-frame1 frame)
  (caddr frame))

; make-frame2 procedure
(define (make-frame2 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

; origin-frame2 procedure
(define (origin-frame2 frame)
  (car frame))

; edge1-frame2 procedure
(define (edge1-frame2 frame)
  (cadr frame))

; edge2-frame2 procedure
(define (edge2-frame2 frame)
  (cddr frame))
