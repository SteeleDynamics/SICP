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

(define (eq-interval? x y)
  (and (= (lower-bound x) (lower-bound y))
       (= (upper-bound x) (upper-bound y))))

(define (make-interval a b)
  (if (<= a b)
      (cons a b)
      (error "make-interval: lower bound greater than upper bound")))

(define (lower-bound ival) (car ival))

(define (upper-bound ival) (cdr ival))

; constructors and selectors
(define (make-center-percent c p)
  (let ((k (* (abs c) (/ p 100.0))))
    (make-interval (- c k) (+ c k))))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2.0))

(define (percent i)
  (define (width i)
    (/ (- (upper-bound i) (lower-bound i)) 2.0))
  (* (/ (width i) (center i)) 100.0))

(define (mul-interval-approx-center x y)
  (* (center x) (center y)))

(define (mul-interval-approx-percent x y)
  (+ (percent x) (percent y)))

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

; par1 and par2 comparison
(define A (make-center-percent 23.5 0.07))
(define B (make-center-percent 31.4 0.16))

(define res1 (par1 A B))
(center res1)
(percent res1)

(define res2 (par2 A B))
(center res2)
(percent res2)

; add'l comparison of arithmetic expressions
(define res3 (div-interval A A))
(center res3)
(percent res3)

(define res4 (div-interval A B))
(center res4)
(percent res4)
