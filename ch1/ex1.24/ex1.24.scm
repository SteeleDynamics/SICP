(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))        

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 2)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (odd? n)
  (= (remainder n 2) 1))

(define (search-for-primes a b)
  (cond ((>= a b) (newline))
        ((odd? a) (timed-prime-test a) (search-for-primes (+ a 2) b))
        (else (search-for-primes (+ a 1) b))))

(search-for-primes 1000 1020)
(search-for-primes 10000 10040)
(search-for-primes 100000 100050)
(search-for-primes 1000000 1000040)

