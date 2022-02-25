; nil defn
(define nil '())

; map procedure
; (define (map proc seq)
;   (if (null? seq)
;       nil
;       (cons (proc (car seq)) (map proc (cdr seq)))))

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

; accumulate-n procedure
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

; dot-product procedure
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

; matrix-*-vector procedure
(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v)) m))

; transpose procedure
(define (transpose mat)
  (accumulate-n cons nil mat))

; matrix-*-matrix procedure
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (map (lambda (col) (dot-product row col)) cols)) m)))

; unit tests for matrix-*-vector, transpose, and matrix-*-matrix
(define A (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))
(define B (list (list 1 2) (list 3 4) (list 5 6) (list 7 8)))
(define v (list 4 3 2 1))
(matrix-*-vector A v)
(transpose A)
(matrix-*-matrix A B)
