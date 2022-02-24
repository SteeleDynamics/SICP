; nil defn
(define nil '())

; reverse iterative procedure
(define (reverse-iter xs)
  (define (iter xs acc)
    (if (null? xs)
        acc
        (iter (cdr xs) (cons (car xs) acc))))
  (iter xs nil))

; reverse recursive procedure
(define (reverse-rec xs)
  (if (null? xs)
      nil
      (append (reverse-rec (cdr xs)) (list (car xs)))))

; deep-reverse iterative procedure
(define (deep-reverse-iter xs)
  (define (iter xs acc)
    (cond ((null? xs) acc)
          ((not (pair? xs)) xs)
          (else (iter (cdr xs)
                      (cons (iter (car xs) nil)
                            acc)))))
  (iter xs nil))

; deep-reverse recursive procedure
(define (deep-reverse-rec xs)
    (cond ((null? xs) nil)
          ((not (pair? xs)) xs)
          (else (append (deep-reverse-rec (cdr xs))
                        (list (deep-reverse-rec (car xs)))))))

; unit tests for reverse-iter and reverse-rec
(reverse-iter '(1 2 3 4))
(reverse-rec '(1 2 3 4))

; unit tests for deep-reverse-iter and deep-reverse-rec
(deep-reverse-iter '((1 2) (3 4)))
(deep-reverse-iter '(1 2 (3 4) 5 (6 (7 8) 9) 10))
(deep-reverse-rec '((1 2) (3 4)))
(deep-reverse-rec '(1 2 (3 4) 5 (6 (7 8) 9) 10))
