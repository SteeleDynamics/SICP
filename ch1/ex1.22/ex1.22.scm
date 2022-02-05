(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes a b)
  (cond ((>= a b) (search-complete))
        ((odd? a) (search-for-primes-iter a b 2))
        (else (search-for-primes (+ a 1) b))))

(define (odd? n)
  (= (remainder n 2) 1))

(define (search-for-primes-iter a b k)
  (timed-prime-test a)
  (search-for-primes (+ a k) b))

(define (search-complete)
  (newline)
  (display "search-complete"))

(search-for-primes 1000 1020)
(search-for-primes 10000 10040)
(search-for-primes 100000 100050)
(search-for-primes 1000000 1000040)
