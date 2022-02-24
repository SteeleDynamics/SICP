; nil def'n
(define nil '())

; fringe proc
(define (fringe xs)
  (define (iter xs acc)
    (cond ((null? xs) acc)
          ((not (pair? xs)) (cons xs acc))
          (else (iter (car xs)
                      (iter (cdr xs) acc)))))
  (iter xs nil))

; fringe unit tests
(define x (list (list 1 2) (list 3 4)))
(fringe x)
(fringe (list x x))
