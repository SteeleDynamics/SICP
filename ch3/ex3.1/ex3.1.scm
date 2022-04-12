(define (make-accumulator init)
  (let ((acc init))
    (lambda (x)
      (begin (set! acc (+ acc x))
             acc))))

; unit tests
(define acc (make-accumulator 0))
(acc 2)
(acc 3)
(acc 5)
(acc 7)
