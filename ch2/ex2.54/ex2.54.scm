; equal? predicate procedure
(define (equal? seq1 seq2)
  (cond ((and (null? seq1) (null? seq2)) true)
        ((and (not (pair? seq1)) (not (pair? seq2)))
         (eq? seq1 seq2))
        ((and (pair? seq1) (pair? seq2))
         (and (equal? (car seq1) (car seq2))
              (equal? (cdr seq1) (cdr seq2))))
        (else false)))

(equal? '(this is a list) '(this is a list))
(equal? '(this is a list) '(this (is a) list))
(equal? '(this (is ()) (a tree)) '(this (is ()) (a tree)))
(equal? '(this (is ()) ((a) tree)) '(this (is ()) (a tree)))
(equal? '((()()(()((())))()(()))()(())) '((()()(()((())))()(()))()(())))
(equal? '((()()(()((())))()(()))()(())) '((()()(()(()))()(()))()(())))