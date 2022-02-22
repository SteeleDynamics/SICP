; map : ('a -> 'b) * ('a list) -> 'b list
(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
            (map proc (cdr items)))))

; square-list1 : not implemented with map HOF
(define (square-list1 items)
  (if (null? items)
      '()
      (cons (square (car items))
            (square-list1 (cdr items)))))

; square-list2 : implemented using map HOF
(define (square-list2 items)
  (map square items))

; unit tests
(square-list1 (list 1 2 3 4))
(square-list2 (list 1 2 3 4))
