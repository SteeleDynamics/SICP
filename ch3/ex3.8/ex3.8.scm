(define (make-fn)
  (let ((x false))
    (lambda (y)
      (cond (x (* x y))
            (else (set! x y) x)))))

; unit tests
(define f1 (make-fn))
(define f2 (make-fn))

; left to right evaluation of (+ (f 0) (f 1))
(let ((a (f1 0)))
  (let ((b (f1 1)))
    (+ a b)))

; right to left evaluation of (+ (f 0) (f 1))
(let ((a (f2 1)))
  (let ((b (f2 0)))
    (+ a b)))

