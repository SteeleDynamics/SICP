(define nil '())

; subsets procedure
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

(subsets (list 1 2 3))

;
; (subsets (list 1 2 3))
; (let ((rest (subsets (list 2 3)))) (append rest (map (lambda (x) (cons 1 x)) rest)))
; (let ((rest (let ((rest (subsets (list 3)))) (append rest (map (lambda (x) (cons 2 x)) rest))))) (append rest (map (lambda (x) (cons 1 x)) rest)))
; (let ((rest (let ((rest (let ((rest (subsets (list)))) (append rest (map (lambda (x) (cons 3 x)) rest))))) (append rest (map (lambda (x) (cons 2 x)) rest))))) (append rest (map (lambda (x) (cons 1 x)) rest)))
; (let ((rest (let ((rest (let ((rest (list nil))) (append rest (map (lambda (x) (cons 3 x)) rest))))) (append rest (map (lambda (x) (cons 2 x)) rest))))) (append rest (map (lambda (x) (cons 1 x)) rest)))
; (let ((rest (let ((rest (append (list nil) (map (lambda (x) (cons 3 x)) (list nil))))) (append rest (map (lambda (x) (cons 2 x)) rest))))) (append rest (map (lambda (x) (cons 1 x)) rest)))
; (let ((rest (let ((rest (append (list nil) (list (list 3))))) (append rest (map (lambda (x) (cons 2 x)) rest))))) (append rest (map (lambda (x) (cons 1 x)) rest)))
; (let ((rest (let ((rest (list nil (list 3)))) (append rest (map (lambda (x) (cons 2 x)) rest))))) (append rest (map (lambda (x) (cons 1 x)) rest)))
; (let ((rest (append (list nil (list 3)) (map (lambda (x) (cons 2 x)) (list nil (list 3)))))) (append rest (map (lambda (x) (cons 1 x)) rest)))
; (let ((rest (append (list nil (list 3)) (list (list 2) (list 2 3))))) (append rest (map (lambda (x) (cons 1 x)) rest)))
; (let ((rest (list nil (list 3) (list 2) (list 2 3)))) (append rest (map (lambda (x) (cons 1 x)) rest)))
; (append (list nil (list 3) (list 2) (list 2 3)) (map (lambda (x) (cons 1 x)) (list nil (list 3) (list 2) (list 2 3))))
; (append (list nil (list 3) (list 2) (list 2 3)) (list (list 1) (list 1 3) (list 1 2) (list 1 2 3)))
; (list nil (list 3) (list 2) (list 2 3) (list 1) (list 1 3) (list 1 2) (list 1 2 3))
; (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))
