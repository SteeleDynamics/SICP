#|
 | Exercise 3.41
 |
 | Ben Bitdiddle worries that it would be better to implement the bank account
 | as follows (where the commented line has been changed):
 |
 | (define (make-account balance)
 |   (define (withdraw amount)
 |     (if (>= balance amount)
 |         (begin (set! balance (- balance amount))
 |                balance)
 |         "Insufficient funds"))
 |   (define (deposit amount)
 |     (set! balance (+ balance amount))
 |     balance)
 |   (let ((protected (make-serializer)))
 |     (define (dispatch m)
 |       (cond ((eq? m 'withdraw) (protected withdraw))
 |             ((eq? m 'deposit) (protected deposit))
 |             ((eq? m 'balance)
 |              ((protected (lambda () balance)))) ; serialized
 |             (else (error "Unknown request -- MAKE-ACCOUNT"
 |                          m))))
 |     dispatch))
 |
 | because allowing unserialized access to the bank balance can result in
 | behavior. Do you agree? Is there any scenario that demonstrates Ben's
 | concern?
 |
 |
 | Answer
 |
 | Yes, I agree. Consider the following scenario:
 |
 | 1. Suppose processes P1 and P2 are processes that send 'withdraw and
 |    'balance messages respectively to an account acc.
 | 2. Now suppose that P1 is changing the value of balance while P2 reads the
 |    value of balance. The value read by P2 would not be guaranteed valid until
 |    P1 is done changing the value.
 |
 | For the expression "(set! balance (-balance amount))" in P1, there is no
 | guarantee that Scheme writes the value atomically. For the expression
 | "balance" in P2, there is no guarantee that Scheme reads the value of balance
 | atomically.
 |
 | Now suppose that reading/writing a 32-bit value uses two 16-bit laod/store
 | instructions respectively. The processor may interleave the loads and stores
 | such that the value obtained in P2 contains 16-bits of the old value and
 | 16-bits of the new value.
 | 
 |#
