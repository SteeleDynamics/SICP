(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average x y) (/ (+ x y) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (id x) x)

(define (repeated f n)
  (define (iter f i acc)
    (if (> i n)
      acc
      (iter f (+ i 1) (compose f acc))))
  (iter f 1 id))

(define (root n)
  (lambda (x)
    (fixed-point-of-transform (lambda (y) (/ x (expt y (- n 1))))
                              (repeated average-damp (quotient n 2))
                              1.0)))

; ((root 4) 81) --> 3
((root 4) 81)

; ((root 9) 40353607) --> 7
((root 9) 40353607)
