#|
 | Exercise 3.48
 |
 | Explain in detail why the deadlock-avoidance method described above,
 | (i.e., the accounts are numbered, and each process attempts to acquire the
 | smaller-numbered account first) avoids deadlock in the exchange problem.
 | Rewrite serialized-exchange to incorporate this idea. (You will also need to
 | modify make-account so that each account is created with a number, which can
 | be accessed by sending an appropriate message.)
 |
 |
 | Answer
 |
 | Given the current implementation of the procedure serialized-exchange:
 |
 | (define (serialized-exchange account1 account2)
 |   (let ((serializer1 (account1 'serializer))
 |         (serializer2 (account2 'serializer)))
 |     ((serializer1 (serializer2 exchange))
 |      account1
 |      account2)))
 |
 | The sequence of evaluation (serializer acquisition) is serializer2, THEN
 | serializer1. So the order of arguments dictates the order of evaluation/
 | acquisition. Suppose processes P1 and P2 try to exchange accounts a1 and a2:
 |
 | (parallel-execute (lambda () (serialized-exchange a1 a2))  ; P1
 |                   (lambda () (serialized-exchange a2 a1))) ; P2
 |
 | Then we encounter the previous deadlock scenario. If accounts have unique
 | identification numbers, acquiring the serializer with lowest identification
 | number first ensures that the previous deadlock scenario cannot occur.
 |
 |#


; make-serializer constructor procedure
(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))

; make-mutex constructor procedure
(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire)))
            ((eq? m 'release) (clear! cell))))
    the-mutex))

; clear! mutator procedure
(define (clear! cell)
  (set-car! cell false))

; test-and-set! atomic mutator procedure
(define (test-and-set! cell)
  (without-interrupts
   (lambda ()
     (if (car cell)
         true
         (begin (set-car! cell true)
                false)))))

; make-counter procedure
(define make-counter
  (lambda ()
    (let ((val 0))
      (lambda ()
        (define res val)
        (set! val (+ val 1))
        res))))

; id-counter generator procedure
(define id-counter (make-counter))

; make-account-and-serializer constructor procedure
(define (make-account-and-serializer balance)
  (define id (id-counter))
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) balance-serializer)
            ((eq? m 'id) id)
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m))))
    dispatch))

; exchange mutator procedure
(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))

; serialized-exchange mutator procedure
(define (serialized-exchange account1 account2)
  (let ((id1 (account1 'id))
        (id2 (account2 'id))
        (serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    ((if (< id1 id2)
         (serializer2 (serializer1 exchange))
         (serializer1 (serializer2 exchange)))
     account1
     account2)))
