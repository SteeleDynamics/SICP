#|
 | Exercise 4.25
 |
 | Suppose that (in ordinary applicative-order Scheme) we define unless as shown
 | above and then define factorial in terms of unless as
 |
 | (define (factorial n)
 |   (unless (= n 1)
 |           (* n (factorial (- n 1)))
 |           1))
 |
 | What happens if we attempt to evaluate (factorial 5)? Will our definitions
 | work in a normal-order language?
 |
 | Answer:
 |
 | If we evaluate (factorial 5) in ordinary applicative-order Scheme, then each
 | operand expression will be evaluated prior to the condition check of the 'if'
 | expression. This will result in an infinite number of evaluations of the
 | 'usual-value' operand expression (* n (factorial (- n 1))), causing Scheme to
 | exceed its maximum recursion depth.
 |
 | However, these definitions WILL work in a normal-order language.
 |#

; unless procedure (applicative-order)
(define (unless condition usual-value exceptional-value)
  (if condition exceptional-value usual-value))

; factorial procedure (applicative-order)
(define (factorial n)
  (unless (= n 1)
          (* n (factorial (- n 1)))
          1))

(factorial 5)

; unless special form (normal-order)
(define-syntax unless
  (syntax-rules ()
    ((unless condition usual-value exceptional-value)
     (if condition exceptional-value usual-value))))

; factorial procedure (normal-order)
(define (factorial n)
  (unless (= n 1)
          (* n (factorial (- n 1)))
          1))

(factorial 5)
