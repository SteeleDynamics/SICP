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

#|
 | Exercise 3.59
 |
 | In section 2.5.3 we saw how to implement a polynomial arithmetic system
 | representing polynomials as lists of terms. In a similar way, we can work
 | with power series, such as
 |
 |                x²    x³     x⁴
 |  e^x = 1 + x + ── + ─── + ───── + ···
 |                2    3·2   4·3·2
 |
 |              x²     x⁴
 |  cos x = 1 - ── + ───── - ···
 |              2    4·3·2
 |
 |                x³      x⁵
 |  sin x  = x - ─── + ─────── - ···
 |               3·2   5·4·3·2
 |
 | represented as infinite streams. We will represent the series a₀ + a₁x +
 | a₂x²+ a₃x³ + ··· as the stream whose elements are the coefficients a₀, a₁,
 | a₂, a₃, ....
 |
 | a. The integral of the series a₀ + a₁x + a₂x² + a₃x³ + ··· is the series
 |
 |           1        1        1
 | c + a₀x + ─ a₁x² + ─ a₂x³ + ─ a₃x⁴ + ···
 |           2        3        4
 |
 | where c is any constant. Define a procedure integrate-series that takes as
 | input a stream a₀, a₁, a₂, ... representing a power series and returns the
 | stream a₀, ½a₁, ⅓a₂, ... of coefficients of the non-constant terms of the
 | integral of the series. (Since the result has no constant term, it doesn't
 | represent a power series; when we use integrate-series, we will cons on the
 | appropriate constant.)
 |
 | b. The function x |-> e^x is its own derivative. This implies that e^x and
 | the integral of ex are the same series, except for the constant term, which
 | is e^0 = 1. Accordingly, we can generate the series for e^x as
 |
 | (define exp-series
 |   (cons-stream 1 (integrate-series exp-series)))
 |
 | Show how to generate the series for sine and cosine, starting from the facts
 | that the derivative of sine is cosine and the derivative of cosine is the
 | negative of sine:
 |
 | (define cosine-series
 |   (cons-stream 1 <??>))
 | (define sine-series
 |   (cons-stream 0 <??>))
 |#

(define harmonic-series
  (stream-map / integers))
(define (integrate-series as)
  (mul-streams harmonic-series as))
(define exp-series
  (cons-stream 1 (integrate-series exp-series)))
(define cosine-series
  (cons-stream 1 (integrate-series (scale-stream sine-series -1))))
(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

(stream-take harmonic-series 8)
(stream-take exp-series 8)
(stream-take cosine-series 8)
(stream-take sine-series 8)
