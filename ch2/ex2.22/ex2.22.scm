; 
; The following procedure produces the answer list in reverse order because
; it iterates through things from front to back, but it accumulates answer by
; cons'ing a list from back to front.

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things) 
              (cons (square (car things))
                    answer))))
  (iter items '()))

(square-list (list 1 2 3 4 5))

;
; The following procedure now produces a mirrored structure of a sequence:
; where the (cdr answer) has value (square item), not (car answer).

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items '()))

(square-list (list 1 2 3 4 5))
