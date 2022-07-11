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

; expand procedure
(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

; harmonic-series stream
(define harmonic-series
  (stream-map / integers))

; integrate series procedure
(define (integrate-series as)
  (mul-streams harmonic-series as))

; exp-series stream (e^x)
(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

; cosine-series stream (cos x)
(define cosine-series
  (cons-stream 1 (integrate-series (scale-stream sine-series -1))))

; sine-series stream (sin x)
(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

; mul-series procedure
(define (mul-series s1 s2)
  (cons-stream
    (* (stream-car s1) (stream-car s2))
    (add-streams (add-streams (scale-stream (stream-cdr s2) (stream-car s1))
                              (scale-stream (stream-cdr s1) (stream-car s2)))
                 (cons-stream 0 (mul-series (stream-cdr s1) (stream-cdr s2))))))

; invert-unit-series procedure
(define (invert-unit-series s)
  (cons-stream 1 (scale-stream
                   (mul-series (stream-cdr s)
                               (invert-unit-series s))
                   -1)))

; div-series procedure
(define (div-series s1 s2)
  (let ((c (stream-car s2)))
    (if (eq? c 0)
        (error "Denominator has zero constant term -- DIV-SERIES" c))
        (mul-series (scale-stream s1 (/ c))
                    (invert-unit-series (scale-stream s2 (/ c))))))

; tangent-series stream (tan x)
(define tangent-series (div-series sine-series cosine-series))

; stream-limit procedure
(define (stream-limit s tol)
  (if (< (abs (- (stream-ref s 1) (stream-ref s 0))) tol)
      (stream-ref s 1)
      (stream-limit (stream-cdr s) tol)))

; euler-transform procedure
(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

; make-tableau procedure
(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform
                             (transform s))))

; acclerated-sequence procedure
(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))

; interleave procedure
(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))
; pairs procedure
(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

; pairs-stream def'n
(define pairs-stream (pairs integers integers))

; pairs-ref procedure
(define (pairs-ref i j)
  (let ((kii (- (expt 2 i) 2))
        (cij (expt 2 i)))
    (if (eq? i j)
        kii
        (+ kii (- (/ cij 2)) (* (- j i) cij)))))

; triples procedure
(define (triples s t u)
  (cons-stream
    (list (stream-car s) (stream-car t) (stream-car u))
    (interleave
      (stream-map (lambda (x) (cons (stream-car s) x))
                  (stream-cdr (pairs t u)))
      (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))

; pythagorean? predicate procedure
(define (pythagorean? x)
  (let ((x2 (map square x)))
    (let ((i2 (car x2)) (j2 (cadr x2)) (k2 (caddr x2)))
      (eq? (+ i2 j2) k2))))

; merge-weighted procedure
(define (merge-weighted weight s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
          (cond ((<= (weight (stream-car s1)) (weight (stream-car s2)))
                 (cons-stream
                   (stream-car s1)
                   (merge-weighted weight (stream-cdr s1) s2)))
                (else
                 (cons-stream
                   (stream-car s2)
                   (merge-weighted weight s1 (stream-cdr s2))))))))

; weighted-pairs
(define (weighted-pairs weight s1 s2)
  (cons-stream
    (list (stream-car s1) (stream-car s2))
    (merge-weighted
      weight
      (stream-map (lambda (x) (list (stream-car s1) x)) (stream-cdr s2))
      (weighted-pairs weight (stream-cdr s1) (stream-cdr s2)))))

; ramanujan-numbers procedure (stream)
(define (ramanujan-numbers)
  (define (weight x)
    (fold-right + 0 (map cube x)))
  (define ord
    (stream-map weight (weighted-pairs weight integers integers)))
  (define (rec a1 ord)
    (let ((a2 (stream-car ord)))
      (if (eq? a1 a2)
          (cons-stream a1 (rec a2 (stream-cdr ord)))
          (rec a2 (stream-cdr ord)))))
  (rec 0 ord))

#|
 | Exercise 3.72
 |
 | In a similar way to exercise 3.71 generate a stream of all numbers that can
 | be written as the sum of two squares in three different ways (showing how
 | they can be so written).
 |#

(define (proc)
  (define (weight x)
    (fold-right + 0 (map square x)))
  (define ord
    (weighted-pairs weight integers integers))
  (define (rec a1 a2 ord)
    (let ((a3 (stream-car ord)))
      (if (= (weight a1) (weight a2) (weight a3))
          (cons-stream
            (list (weight a1) (list a1 a2 a3))
            (rec a2 a3 (stream-cdr ord)))
          (rec a2 a3 (stream-cdr ord)))))
  (rec (list 0 0) (stream-car ord) (stream-cdr ord)))

(stream-take (proc) 8)
