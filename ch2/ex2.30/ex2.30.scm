(define nil '())

(define (scale-tree tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree sub-tree factor)
             (* sub-tree factor)))
       tree))

(define (square-tree1 tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree1 (car tree))
                    (square-tree1 (cdr tree))))))

(square-tree1
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))

(define (square-tree2 tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree2 sub-tree)
             (square sub-tree)))
       tree))

(square-tree2
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))
