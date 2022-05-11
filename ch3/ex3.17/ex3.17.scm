; memq procedure
(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

; count-pairs procedure
(define (count-pairs x)
  (define visited '())
  (define (visit-pairs u)
    (cond ((not (pair? u)) 0)
          ((memq u visited) 0)
          (else
            (set! visited (cons u visited))
            (+ (visit-pairs (car u))
               (visit-pairs (cdr u))
               1))))
  (visit-pairs x))

; unit tests
(define x (list 'a 'b 'c))
(count-pairs x)

(define x (list 'a 'b))
(define y (cons x (cdr x)))
(count-pairs y)

(define x (list 'a))
(define y (cons x x))
(define z (cons y y))
(count-pairs z)

(define x (list 'a 'b 'c))
(set-cdr! (last-pair x) x)
(count-pairs x)
