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

; Carmichael Numbers --> false
(map miller-rabin-test (list 561 1105 1729 2465 2821 6601))

; Known Prime Numbers --> true
(map miller-rabin-test (list 563 1109 1733 2467 2833 6607))
