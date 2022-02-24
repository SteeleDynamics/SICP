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

; implement map using accumulate
(define (map* proc seq)
  (accumulate (lambda (x y) (cons (proc x) y)) nil seq))

; implement append using accumulate
(define (append* seq1 seq2)
  (accumulate cons seq2 seq1))

; implement length using accumulate
(define (length* seq)
  (accumulate (lambda (x y) (+ y 1)) 0 seq))

; unit tests for map*, append*, and length*
(map* (lambda (x) (+ x 1)) (list 0 1 2 3))
(append* (list 1 2 3 4) (list 5 6 7 8))
(length* (list 2 4 8 16 32 64 128))
