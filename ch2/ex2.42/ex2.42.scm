; nil defn
(define nil '())

; append procedure
(define (append seq1 seq2)
  (if (null? seq1)
      seq2
      (cons (car seq1) (append (cdr seq1) seq2))))

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

; flatmap procedure
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

; queens procedure
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

; empty-board defn
(define empty-board nil)

; adjoin-position procedure
(define (adjoin-position new-row k rest-of-queens)
  (cons (list new-row k) rest-of-queens))

; safe? predicate procedure
(define (safe? k positions)
  (if (= k 0)
      true
      (let ((q (car positions)))
        (fold-left
          (lambda (acc p)
            (and acc
                 (not (= (car q) (car p)))
                 (not (= (abs (/ (- (cadr q) (cadr p))
                                 (- (car q) (car p))))
                         1))))
          true
          (cdr positions)))))

; unit tests for queens
(length (queens 3))
(length (queens 4))
(length (queens 5))
(length (queens 6))
(length (queens 7))
(length (queens 8))
(length (queens 9))
(length (queens 10))
(length (queens 11))
