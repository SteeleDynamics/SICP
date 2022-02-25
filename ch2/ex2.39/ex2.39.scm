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

; accumulate-n procedure
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

; fold-right procedure (formerly known as accumulate)
(define (fold-right op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (fold-right op init (cdr seq)))))

; fold-left procedure
(define (fold-left op init seq)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter init seq))

; reverse-right procedure
(define (reverse-right seq)
  (fold-right (lambda (x y) (append y (list x))) nil seq))

; reverse-left procedure
(define (reverse-left seq)
  (fold-left (lambda (x y) (cons y x)) nil seq))

; unit tests for reverse-left and reverse-right
(reverse-right (list 1 2 3 4))
(reverse-left (list 1 2 3 4))
