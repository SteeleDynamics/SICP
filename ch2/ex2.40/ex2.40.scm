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

; flatmap procedure
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

; [ch1/ex1.21] smallest-divisor procedure
(define (smallest-divisor n)
  (find-divisor n 2))

; [ch1/ex1.21] find-divisor procedure
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

; [ch1/ex1.21] divides? predicate procedure
(define (divides? a b)
  (= (remainder b a) 0))

; [ch1/ex1.21] prime? predicate procedure
(define (prime? n)
  (= n (smallest-divisor n)))

; prime-sum? procedure
(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

; make-pair-sum procedure
(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

; unique-pairs procedure
(define (unique-pairs n)
  (flatmap (lambda (i) (map (lambda (j) (list i j))
                            (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

; prime-sum-pairs procedure
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))

; unit tests for unique-pairs and prime-sum-pairs
(unique-pairs 6)
(prime-sum-pairs 6)
