#|
 | Exercise 4.36
 |
 | Exercise 3.69 discussed how to generate the stream of all Pythagorean
 | triples, with no upper bound on the size of the integers to be searched.
 | Explain why simply replacing 'an-integer-between' by
 | 'an-integer-starting-from' in the procedure in exercise 4.35 is not an
 | adequate way to generate arbitrary Pythagorean triples. Write a procedure
 | that actually will accomplish this. (That is, write a procedure for which
 | repeatedly typing 'try-again' would in principle eventually generate all
 | Pythagorean triples.)
 |
 | Answer
 |
 | Suppose we implemented the procedure as described in the problem statement:
 |
 | (define (a-pythagorean-triple-starting-from n)
 |   (let ((i (an-integer-starting-from n)))
 |     (let ((j (an-integer-starting-from i)))
 |       (let ((k (an-integer-starting-from j)))
 |         (require (= (+ (* i i) (* j j)) (* k k)))
 |         (list i j k)))))
 |
 | Starting with n = 1, the resulting sequence of possible (i j k) values are:
 |
 | ⟨(1 1 1) (1 1 2) (1 1 3) ...⟩
 |
 | After each 'require' expression failure, the most recent choice point 'k' is
 | the only value that is explored. Neither 'i' nor 'j' are explored because 'k'
 | will increment forever. This is nearly identical to the problem explored in
 | §3.5.3 'Exploiting the Stream Paradigm' -- 'Infinite streams of pairs'.
 |#

; an-integer-starting-from ambiguous procedure
(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (+ n 1))))

; interleave ambiguous procedure
(define (interleave a1 a2)
  (amb a1 (interleave a2 a1)))

; a-pair-starting-from ambiguous procedure
(define (a-pair-starting-from n)
  (interleave (list n (an-integer-starting-from n))
              (a-pair-starting-from (+ n 1))))

; a-triple-starting-from ambiguous procedure
(define (a-triple-starting-from n)
  (interleave (cons n (a-pair-starting-from n))
              (a-triple-starting-from (+ n 1))))

; a-pythagorean-triple-starting-from ambiguous procedure
(define (a-pythagorean-triple-starting-from n)
  (let ((t (a-triple-starting-from n)))
    (let ((i (car t)) (j (cadr t)) (k (caddr t)))
      (require (= (+ (* i i) (* j j)) (* k k)))
      t)))
