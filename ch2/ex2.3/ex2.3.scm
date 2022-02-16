(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (area-rect1 rect)
  (* (width-rect1 rect)
     (height-rect1 rect)))

(define (perimeter-rect1 rect)
  (* (+ (width-rect1 rect)
        (height-rect1 rect))
     2))

(define (make-rect1 bot-left top-right)
  (cons bot-left top-right))

(define (width-rect1 rect)
  (- (cadr rect) (caar rect)))

(define (height-rect1 rect)
  (- (cddr rect) (cdar rect)))

(define r1 (make-rect1 (make-point 3 2) (make-point 10 7)))

(width-rect1 r1)
(height-rect1 r1)
(area-rect1 r1)
(perimeter-rect1 r1)

(define (area-rect2 rect)
  (* (width-rect2 rect)
     (height-rect2 rect)))

(define (perimeter-rect2 rect)
  (* (+ (width-rect2 rect)
        (height-rect2 rect))
     2))

(define (make-rect2 bot-left top-right)
  (cons bot-left
        (cons (- (x-point top-right)
                 (x-point bot-left))
              (- (y-point top-right)
                 (y-point bot-left)))))

(define (width-rect2 rect) (cadr rect))

(define (height-rect2 rect) (cddr rect))

(define r2 (make-rect2 (make-point 3 2) (make-point 10 7)))

(width-rect2 r2)
(height-rect2 r2)
(area-rect2 r2)
(perimeter-rect2 r2)
