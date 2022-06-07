#|
 | Exercise 3.39
 |
 | Which of the five possibilities in the parallel execution shown above remain
 | if we instead serialize execution as follows:
 |
 | (define x 10)
 |
 | (define s (make-serializer))
 |
 | (parallel-execute (lambda () (set! x ((s (lambda () (* x x))))))
 |                   (s (lambda () (set! x (+ x 1)))))
 |
 | 
 | Answer
 |
 | 100: P1 accesses x (twice), then P2 sets x to 11, then P1 sets x
 | 101: P1 sets x to 100, then P2 increments x to 101
 | 121: P2 increments x to 11, then P1 sets x to x times x
 | 
 | The serializer guarantees:
 | 1. The two accesses of x in P1 do not interleave with P2, eliminating 110.
 | 2. The access and setting of x in P2 isn't interrupted by P1, eliminating 11.
 |#
