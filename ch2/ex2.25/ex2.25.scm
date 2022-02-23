; (1 3 (5 7) 9)
(define a (cons 1 (cons 3 (cons (cons 5 (cons 7 '())) (cons 9 '())))))
a
(car (cdr (car (cdr (cdr a)))))

; ((7))
(define b (cons (cons 7 '()) '()))
b
(car (car b))

; (1 (2 (3 (4 (5 (6 7))))))
(define c (cons 1 (cons (cons 2 (cons (cons 3 (cons (cons 4 (cons (cons 5 (cons (cons 6 (cons 7 '())) '())) '())) '())) '())) '())))
c
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr c))))))))))))
