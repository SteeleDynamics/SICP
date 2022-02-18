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
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(define (width-interval ival)
  (/ (- (upper-bound ival) (lower-bound ival)) 2.0))

(define (make-interval a b) (cons a b))
(define (lower-bound ival) (car ival))
(define (upper-bound ival) (cdr ival))

(define ival1 (make-interval 2 3))
(define ival2 (make-interval 5 7))

; unit tests for arithmetic ops for intervals
(define res1 (add-interval ival1 ival2))
(define res2 (sub-interval ival2 ival1))
(define res3 (mul-interval ival1 ival2))
(define res4 (div-interval ival1 ival2))

; unit tests for width op for intervals
(width-interval res1)
(+ (width-interval ival1) (width-interval ival2))
(width-interval res2)
(- (width-interval ival2) (width-interval ival1))
(width-interval res3)
(* (width-interval ival1) (width-interval ival2))
(width-interval res4)
(/ (width-interval ival1) (width-interval ival2))
