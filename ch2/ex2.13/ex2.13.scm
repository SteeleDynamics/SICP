;
; Intervals A and B are defined as follows:

; A ==> C_A ± p_A %
; B ==> C_B ± p_B %
;
; The "radii" for intervals A and B are respectively:

; r_A = (p_A / 100) * C_A
; r_B = (p_B / 100) * C_B
;
; The intervals A and B have lower and upper bounds of:
;
; A : [C_A - r_A, C_A + r_A]
; B : [C_B - r_B, C_B + r_B]
;
; We can assume that p_A, p_B are small (given) and all variables are positive
; integers (given). Calculating the product of intervals A nd B yields:
;
; A * B = [(C_A - r_A) * (C_B - r_B), (C_A - r_A) * (C_B - r_B)]
;       = [(C_A * C_B) - (C_A * r_B) - (C_B * r_A) + (r_A * r_b),
;          (C_A * C_B) + (C_A * r_B) + (C_B * r_A) + (r_A * r_b)]
;       = [(C_A * C_B) - (C_A * C_B * (p_B / 100)) - (C_A * C_B * (p_A / 100)) + (C_A * C_B * (p_A / 100) * (p_B / 100)),
;          (C_A * C_B) + (C_A * C_B * (p_B / 100)) + (C_A * C_B * (p_A / 100)) + (C_A * C_B * (p_A / 100) * (p_B / 100))]
;       = [(C_A * C_B) - (C_A * C_B * ((p_B + p_A) / 100) + (C_A * C_B * (p_A / 100) * (p_B / 100)),
;          (C_A * C_B) + (C_A * C_B * ((p_B + p_A) / 100) + (C_A * C_B * (p_A / 100) * (p_B / 100))]
;
; Since the product of two small numbers is a very small number, the last term
; in each bound is negligible. This yields the following:
;
; A * B ~ [(C_A * C_B) - (C_A * C_B) * ((p_A + p_B) / 100),
;          (C_A * C_B) + (C_A * C_B) * ((p_A + p_B) / 100)]
;
; We can now see the same pattern as before with the individual intervals:
;
; C_AB ~ C_A * C_B
; p_AB ~ p_A + p_B
; r_AB ~ C_AB * (p_AB / 100)
;
; Substitution yields:
;
; A * B ~ [C_AB - r_AB, C_AB + r_AB]
;

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

(define i1 (make-center-percent 23.5 0.07))
(define i2 (make-center-percent 31.4 0.16))
(define i3 (mul-interval i1 i2))
(center i3)
(percent i3)

(mul-interval-approx-center i1 i2)
(mul-interval-approx-percent i1 i2)
