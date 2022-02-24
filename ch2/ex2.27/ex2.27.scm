; nil def'n
(define nil '())

; reverse procedure
(define (reverse xs)
  (define (iter xs acc)
    (if (null? xs)
        acc
        (iter (cdr xs) (cons (car xs) acc))))
  (iter xs nil))

; deep-reverse procedure
(define (deep-reverse xs)
  (define (iter xs acc)
    (cond ((null? xs) acc)
          ((not (pair? xs)) xs)
          (else (iter (cdr xs)
                      (cons (iter (car xs) nil)
                            acc)))))
  (iter xs nil))

; unit tests for reverse and deep-reverse
(deep-reverse '((1 2) (3 4)))
(deep-reverse '(1 2 (3 4) 5 (6 (7 8) 9) 10))
