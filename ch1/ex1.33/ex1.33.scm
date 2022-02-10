; filtered-accumulate combines all values a < b such that (pred? a) --> true
(define (filtered-accumulate pred? combiner null-value term a next b)
  (define (iter a result)
    (cond ((> a b) result)
          ((pred? a) (iter (next a) (combiner (term a) result)))
          (else (iter (next a) result))))
  (iter a null-value))

; check for non-trivial square root
(define (non-trivial-sqrt k n)
  (define r (remainder (square k) n))
  (if (and (not (= k 1))
           (not (= k (- n 1)))
           (= r 1))
      0
      r))

; expmod --> (base ^ exp) mod m, includes non-trivial square root check
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (non-trivial-sqrt (expmod base (/ exp 2) m) m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

; Miller-Rabin Test
(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))

; determine if a and b are relatively prime
(define (rel-prime? b) 
  (lambda (a) (= (gcd a b) 1)))

; increment an integer
(define (incr x) (+ x 1))

; identity function
(define (id x) x)

; ssq-prime --> (+ (* p1 p1) ... (* pk pk)) for all pk primes in [a,b]
(define (ssq-prime a b)
  (filtered-accumulate miller-rabin-test + 0 square a incr b))

; prod-rel-prime --> (* r1 r2 ... rk) for all rk < n such that gcd (rk,n) = 1
(define (prod-rel-prime n)
  (filtered-accumulate (rel-prime? n) * 1 id 1 incr n))

; 4 + 9 + 25 + 49 + 121 + 169 + 289 + 361 + 529 + 841 + 961 = 3358
(ssq-prime 2 32)

; 1 * 2 * 4 * 7 * 8 * 11 * 13 * 14 = 896896
(prod-rel-prime 15)
