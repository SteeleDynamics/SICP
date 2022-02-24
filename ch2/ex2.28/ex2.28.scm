; nil defn
(define nil '())

; fringe iterative process
(define (fringe-iter xs)
  (define (iter xs acc)
    (cond ((null? xs) acc)
          ((not (pair? xs)) (cons xs acc))
          (else (iter (car xs)
                      (iter (cdr xs) acc)))))
  (iter xs nil))

; fringe recursive process
(define (fringe-rec xs)
  (cond ((null? xs) nil)
        ((not (pair? xs)) (list xs))
        (else (append (fringe-rec (car xs))
                      (fringe-rec (cdr xs))))))

; fringe-iter and fringe-rec unit tests
(define x (list (list 1 2) (list 3 4)))
(fringe-iter x)
(fringe-iter (list x x))
(fringe-rec x)
(fringe-rec (list x x))
