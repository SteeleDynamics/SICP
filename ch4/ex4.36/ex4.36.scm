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
 |
 | See exercise 4.38 for an implementation of the ambiguous evaluator. The
 | following procedures would be evaluated within the ambiguous evaluator.
 |
 | IDEA: We must create a traversal strategy to iterate over all ordered pairs
 | (i, j) such that 0 < i <= j. We can do this by traversing all of the ordered
 | pairs in a column-major order (denoted by the alphabetical labels below). A
 | similar strategy will work for ordered triples.
 |
 | A:(1,1)  B:(1,2)   D:(1,3)   G:(1,4)
 |          C:(2,2)   E:(2,3)   H:(2,4)   ...
 |                    F:(3,3)   I:(3,4)
 |                              J:(4,4)
 |#

; require procedure
(define (require p)
  (if (not p) (amb)))

; an-integer-starting-from ambiguous procedure
(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (+ n 1))))

; an-integer-between ambiguous procedure
(define (an-integer-between low high)
  (require (>= high low))
  (amb low (an-integer-between (+ low 1) high)))

; a-pythagorean-triple-starting-from ambiguous procedure
(define (a-pythagorean-triple-starting-from n)
  (let ((k (an-integer-starting-from n)))
    (let ((j (an-integer-between 1 k)))
      (let ((i (an-integer-between 1 j)))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))
