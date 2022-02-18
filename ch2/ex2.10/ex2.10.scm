(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (if (and (<= (lower-bound y) 0)
           (>= (upper-bound y) 0))
      (error "div-interval: divisor interval spans zero")
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

(define (width-interval ival)
  (/ (- (upper-bound ival) (lower-bound ival)) 2.0))

(define (make-interval a b) (cons a b))
(define (lower-bound ival) (car ival))
(define (upper-bound ival) (cdr ival))

(define ival1 (make-interval 2 3))
(define ival2 (make-interval 5 7))
(define ival3 (make-interval -1 4))

; unit tests for div-interval
(div-interval ival1 ival2)
(div-interval ival1 ival3)
(restart 1)
