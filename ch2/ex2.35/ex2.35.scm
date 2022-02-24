; nil defn
(define nil '())

; map procedure
(define (map proc seq)
  (if (null? seq)
      nil
      (cons (proc (car seq)) (map proc (cdr seq)))))

; filter procedure
(define (filter pred seq)
  (cond ((null? seq) nil)
        ((pred (car seq))
         (cons (car seq)
               (filter pred (cdr seq))))
        (else (filter pred (cdr seq)))))

; accumulate procedure
(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (accumulate op init (cdr seq)))))

; enumerate-interval procedure
(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

; enumerate-tree procedure
(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

; implement count-leaves using accumulate and map
(define (count-leaves tree)
  (accumulate + 0 (map (lambda (sub-tree)
                         (cond ((null? sub-tree) 0)
                               ((not (pair? sub-tree)) 1)
                               (else (count-leaves sub-tree))))
                       tree)))

; unit test for count-leaves
(define x (cons (list 1 2) (list 3 4)))
(length x)
(count-leaves x)
(list x x)
(length (list x x))
(count-leaves (list x x))
