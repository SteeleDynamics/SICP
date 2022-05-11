; memq procedure
(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

; cycle? procedure
(define (cycle? x)
  (define visited '())
  (define (visit-pairs u)
    (cond ((not (pair? u)) false)
          ((memq u visited) true)
          (else
            (set! visited (cons u visited))
            (or (visit-pairs (car u))
                (visit-pairs (cdr u))))))
  (visit-pairs x))

; unit tests
(define x (list 'a 'b 'c))
(cycle? x)

(define x (list 'a 'b))
(define y (cons x (cdr x)))
(cycle? y)

(define x (list 'a))
(define y (cons x x))
(define z (cons y y))
(cycle? z)

(define x (list 'a 'b 'c))
(set-cdr! (last-pair x) x)
(cycle? x)

(define x (cons 'a 'b))
(define y (cons 'c 'd))
(define z (cons x y))
(cycle? z)
