; element-of-set? predicate procedure
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

; adjoin-set procedure
(define (adjoin-set x set)
  (cond ((null? set) (cons x set))
        ((< x (car set)) (cons x set))
        ((= x (car set)) set)
        (else (cons (car set) (adjoin-set x (cdr set))))))

; intersection-set procedure
(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))

; union-set procedure
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((< (car set1) (car set2)) (cons (car set1)
                                         (union-set (cdr set1) set2)))
        ((= (car set1) (car set2)) (union-set (cdr set1) set2))
        ((> (car set1) (car set2)) (cons (car set2)
                                         (union-set set1 (cdr set2))))))

; unit tests
(union-set '(1 4 5 7 8) '(2 3 5 6))
(union-set '(10 11 13 14) '(9 12 13 15 16))
