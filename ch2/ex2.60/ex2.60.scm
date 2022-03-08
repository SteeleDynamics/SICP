; element-of-set? predicate procedure
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

; adjoin-set procedure
(define (adjoin-set x set)
      (cons x set))

; intersection-set procedure
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

; union-set procedure
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        (else (cons (car set1)
                    (union-set (cdr set1) set2)))))

; unit tests
(element-of-set? 4 '(2 1 8 4))
(element-of-set? 3 '(2 1 8 4))
(adjoin-set 4 '(2 1 8 4))
(adjoin-set 3 '(2 1 8 4))
(intersection-set '(1 2 3 4) '(3 4 5 6))
(intersection-set '() '(5 6 7 8))
(intersection-set '(1 2 3 4) '())
(intersection-set '(1 2 3 4) '(5 6 7 8))
(union-set '(1 2 3 4) '(3 4 5 6))
(union-set '() '(5 6 7 8))
(union-set '(1 2 3 4) '())
(union-set '(1 2 3 4) '(5 6 7 8))
