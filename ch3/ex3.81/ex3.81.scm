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

; integral procedure
(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)

; square-wave-gen procedure
(define (square-wave-gen a b c d)
  (lambda (x) (+ (* a (expt -1 (integer-floor (* 2 (- x b)) c))) d)))

; random-in-range procedure (from exercise 3.5)
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

; random-in-range-stream procedure
(define (random-in-range-stream low high)
  (cons-stream (random-in-range low high)
               (random-in-range-stream low high)))

; sense-data def'n
(define sense-data (random-in-range-stream -512 513))

; sign-change-detector procedure
(define (sign-change-detector a d)
  (cond ((and (>= a 0) (>= d 0)) 0)
        ((>= a 0) -1)
        ((>= d 0) 1)
        (else 0)))

; smooth procedure
(define (smooth input-stream)
  (stream-map
    (lambda (x) (integer-floor x 2))
    (add-streams input-stream (stream-cdr input-stream))))

; make-zero-crossings procedure (refactored)
(define (make-zero-crossings input-stream smooth)
  (let ((smoothed (smooth input-stream)))
    (stream-map
      sign-change-detector
      smoothed
      (stream-cdr smoothed))))

; integral procedure (delayed-integrand, implicit impl)
(define (integral delayed-integrand initial-value dt)
  (define int
    (cons-stream
      initial-value
      (let ((integrand (force delayed-integrand)))
        (add-streams (scale-stream integrand dt)
                     int))))
  int)

; solve procedure
(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

; solve-2nd procedure (explicit impl)
(define (solve-2nd a b dt y0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (add-streams (scale-stream dy a)
                           (scale-stream y b)))
  y)

; solve-2nd procedure (generalized impl)
(define (solve-2nd f y0 dy0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (stream-map f dy y))
  y)

; RLC procedure
(define (RLC R L C dt)
  (lambda (vC0 iL0)
    (define vC (integral (delay dvC) vC0 dt))
    (define iL (integral (delay diL) iL0 dt))
    (define dvC (scale-stream iL (/ -1 C)))
    (define diL (add-streams (scale-stream vC (/ L))
                             (scale-stream iL (/ (- R) L))))
    (cons vC iL)))

; rand-init value (exercise 3.6)
(define rand-init 107)

; rand-update procedure (exercise 3.6)
(define (rand-update x)
  (let ((a 41) (b 71) (m 2053))
    (modulo (+ (* a x) b) m)))))

; rand procedure (exercise 3.6)
(define rand
  (let ((x rand-init))
    (lambda (m)
      (cond ((eq? m 'generate) (set! x (rand-update x)) x)
            ((eq? m 'reset) (lambda (y) (set! x y) x))
            (else (error "message not understood -- RAND"
                         m))))))

; random-numbers stream
(define random-numbers
  (cons-stream rand-init
               (stream-map rand-update random-numbers)))

; map-successive-pairs procedure
(define (map-successive-pairs f s)
  (cons-stream
   (f (stream-car s) (stream-car (stream-cdr s)))
   (map-successive-pairs f (stream-cdr (stream-cdr s)))))

; cesaro-stream
(define cesaro-stream
  (map-successive-pairs (lambda (r1 r2) (= (gcd r1 r2) 1))
                        random-numbers))

; monte-carlo procedure
(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
     (/ passed (+ passed failed))
     (monte-carlo
      (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

; pi estimate stream using monte-carlo and cesaro-stream
(define pi
  (stream-map (lambda (p) (sqrt (/ 6 p)))
              (monte-carlo cesaro-stream 0 0)))

#|
 | Exercise 3.81
 |
 | Exercise 3.6 discussed generalizing the random-number generator to allow
 | one to reset the random-number sequence so as to produce repeatable
 | sequences of "random" numbers. Produce a stream formulation of this same
 | generator that operates on an input stream of requests to 'generate' a new
 | random number or to 'reset' the sequence to a specified value and that
 | produces the desired stream of random numbers. Don't use assignment in
 | your solution.
 |#

; rand-nums-from-reqs procedure
(define (rand-nums-from-reqs reqs)
  (define (generate? x)
    (eq? x 'generate))
  (define (reset? x)
    (and (pair? x)
         (eq? (car x) 'reset)
         (integer? (cadr x))))
  (define (proc-req req x)
    (cond ((generate? req) (rand-update x))
          ((reset? req) (cadr req))
          (else (error "invalid request -- PROC-REQ" req))))
  (define rand-nums
    (cons-stream rand-init
                 (stream-map proc-req reqs rand-nums)))
  rand-nums)

(define reqs
  (cons-stream 'generate
  (cons-stream 'generate
  (cons-stream '(reset 107)
  (cons-stream 'generate
  (cons-stream 'generate
  (cons-stream '(reset 1009)
  (cons-stream 'generate
  (cons-stream 'generate
               the-empty-stream)))))))))

(stream-take (rand-nums-from-reqs reqs) 9)
