; rand-init value
(define rand-init 107)

; rand-update procedure
(define (rand-update x)
  (let ((a 41) (b 71) (m 2053))
    (modulo (+ (* a x) b) m)))))

; rand procedure
(define rand
  (let ((x rand-init))
    (lambda (m)
      (cond ((eq? m 'generate) (set! x (rand-update x)) x)
            ((eq? m 'reset) (lambda (y) (set! x y) x))
            (else (error "message not understood -- RAND"
                         m))))))

; unit-tests
(rand 'generate)
(rand 'generate)
(rand 'generate)
((rand 'reset) 107)
(rand 'generate)
(rand 'generate)
(rand 'generate)
((rand 'reset) 1009)
(rand 'generate)
(rand 'generate)
(rand 'generate)
(rand 'foo)
(restart 1)
