; memo-proc procedure
(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? true)
                 result)
          result))))

#|
 | §3.5.1 Footnote 56:
 |
 | Although stream-car and stream-cdr can be defined as procedures, cons-stream
 | must be a special form. If cons-stream were a procedure, then, according to
 | our model of evaluation, evaluating (cons-stream <a> <b>) would automatically
 | cause <b> to be evaluated, which is precisely what we do not want to happen.
 | For the same reason, delay must be a special form, though force can be an
 | ordinary procedure.
 |#

; delay procedure -- create thunk
; (define (delay expr) (lambda () expr))
; (define (delay expr) (memo-proc (lambda () expr)))
(define-syntax delay
  (syntax-rules ()
    ((delay ?expr) (memo-proc (lambda () ?expr)))))

; force procedure -- evaluate thunk
(define (force delayed-expr) (delayed-expr))

; the-empty-stream defn
(define the-empty-stream '())

; cons-stream constructor procedure
; (define (cons-stream a b) (cons a (delay b)))
(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream a b) (cons a (delay b)))))

; stream-car selector procedure
(define (stream-car stream) (car stream))

; stream-cdr selector procedure
(define (stream-cdr stream) (force (cdr stream)))

; stream-null? predicate procedure
(define (stream-null? stream) (null? stream))

; stream-ref procedure
(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

#|
 | ; stream-map procedure
 | (define (stream-map proc s)
 |   (if (stream-null? s)
 |       the-empty-stream
 |       (cons-stream (proc (stream-car s))
 |                    (stream-map proc (stream-cdr s)))))
 |#

; stream-for-each procedure
(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))
#|
 | ; display-stream procedure
 | (define (display-stream s)
 |   (stream-for-each display-line s))
 |
 | ; display-line procedure
 | (define (display-line x)
 |   (newline)
 |   (display x))
 |#

; display-stream procedure (instructor's manual, augmented)
(define display-stream
  (let ()
    (define (iter stream delim)
      (cond ((stream-null? stream) (display "]"))
            (else (display delim)
                  (display (stream-car stream))
                  (iter (stream-cdr stream) " "))))
    (lambda (stream)
      (newline)
      (display "[")
      (iter stream ""))))

; stream-enumerate-interval procedure
(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

; stream-filter procedure
(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

; stream-map procedure
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
        (apply proc (map stream-car argstreams))
        (apply stream-map
          (cons proc (map stream-cdr argstreams))))))

; stream-take procedure
(define (stream-take s n)
  (cond ((< n 0) (error "Subscript"))
        ((= n 0) '())
        ((stream-null? s) (error "Subscript"))
        (else (cons (stream-car s)
                    (stream-take (stream-cdr s) (- n 1))))))

; integers-starting-from procedure
(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

; divisible? predicate procedure
(define (divisible? x y) (= (remainder x y) 0))

; (Sieve of Eratosthenes) sieve procedure
(define (sieve stream)
  (cons-stream
   (stream-car stream)
   (sieve (stream-filter
           (lambda (x)
             (not (divisible? x (stream-car stream))))
           (stream-cdr stream)))))

; (Sieve of Eratosthenes) primes stream
(define primes (sieve (integers-starting-from 2)))

; ones stream
(define ones (cons-stream 1 ones))

; add-streams procedure
(define (add-streams s1 s2)
  (stream-map + s1 s2))

; integers stream
(define integers (cons-stream 1 (add-streams ones integers)))

; fibs stream
(define fibs
  (cons-stream 0 (cons-stream 1 (add-streams (stream-cdr fibs) fibs))))

; scale-stream procedure
(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

; mul-streams procedure
(define (mul-streams s1 s2)
  (stream-map * s1 s2))

; partial-sums procedure
(define (partial-sums s)
  (add-streams s (cons-stream 0 (partial-sums s))))

#|
 | Exercise 3.56
 |
 | A famous problem, first raised by R. Hamming, is to enumerate, in ascending
 | order with no repetitions, all positive integers with no prime factors other
 | than 2, 3, or 5. One obvious way to do this is to simply test each integer in
 | turn to see whether it has any factors other than 2, 3, and 5. But this is
 | very inefficient, since, as the integers get larger, fewer and fewer of them
 | fit the requirement. As an alternative, let us call the required stream of
 | numbers S and notice the following facts about it.
 |
 | * S begins with 1.
 | * The elements of (scale-stream S 2) are also elements of S.
 | * The same is true for (scale-stream S 3) and (scale-stream 5 S).
 | * These are all the elements of S.
 |
 | Now all we have to do is combine elements from these sources. For this we
 | define a procedure merge that combines two ordered streams into one ordered
 | result stream, eliminating repetitions:
 |
 | (define (merge s1 s2)
 |   (cond ((stream-null? s1) s2)
 |         ((stream-null? s2) s1)
 |         (else
 |          (let ((s1car (stream-car s1))
 |                (s2car (stream-car s2)))
 |            (cond ((< s1car s2car)
 |                   (cons-stream s1car (merge (stream-cdr s1) s2)))
 |                  ((> s1car s2car)
 |                   (cons-stream s2car (merge s1 (stream-cdr s2))))
 |                  (else
 |                   (cons-stream s1car
 |                                (merge (stream-cdr s1)
 |                                       (stream-cdr s2)))))))))
 |
 | Then the required stream may be constructed with merge, as follows:
 |
 | (define S (cons-stream 1 (merge <??> <??>)))
 |
 | Fill in the missing expressions in the places marked <??> above.
 |#


; merge procedure
(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream s1car (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream s2car (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream s1car
                               (merge (stream-cdr s1)
                                      (stream-cdr s2)))))))))

; Hamming stream
(define S (cons-stream 1 (merge (scale-stream S 2)
                                (merge (scale-stream S 3)
                                       (scale-stream S 5)))))
(stream-take S 42)
