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

; ordered-triples procedure
(define (ordered-triples n)
  (let ((nats-n (enumerate-interval 1 n)))
    (flatmap (lambda (i)
               (flatmap (lambda (j)
                          (map (lambda (k) (list i j k))
                               nats-n))
                        nats-n))
             nats-n)))

; distinct-ordered-triples procedure
(define (distinct-ordered-triples n)
  (let ((pred (lambda (t)
                (let ((i (car t)) (j (cadr t)) (k (caddr t)))
                  (and (not (= i j)) (not (= i k)) (not (= j k)))))))
    (filter pred (ordered-triples n))))

; distinct-ordered-triples-sum procedure
(define (distinct-ordered-triples-sum n s)
  (let ((pred (lambda (t)
                (let ((i (car t)) (j (cadr t)) (k (caddr t)))
                  (= (+ i j k) s)))))
    (filter pred (distinct-ordered-triples n)))) 

; distinct-ordered-triples-sum unit test
(distinct-ordered-triples-sum 5 8)
