#|
 | Exercise 3.47
 |
 | A semaphore (of size n) is a generalization of a mutex. Like a mutex, a
 | semaphore supports acquire and release operations, but it is more general in
 | that up to n processes can acquire it concurrently. Additional processes that
 | attempt to acquire the semaphore must wait for release operations. Give
 | implementations of semaphores
 |
 | a. in terms of mutexes
 |
 | b. in terms of atomic test-and-set! operations.
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

; make-semaphore-1 constructor procedure (uses mutex)
(define (make-semaphore-1 n)
  (let ((count 0)
        (mutex (make-mutex)))
    (define (add-count! k)
      (mutex 'acquire)
      (set! count (+ count k))
      (mutex 'release))
    (define (the-semaphore m)
      (cond ((and (eq? m 'acquire) (< count n)) (add-count! 1))
            ((eq? m 'acquire) (the-semaphore m))
            ((and (eq? m 'release) (> count 0)) (add-count! -1))
            ((eq? m 'release) unspecific)))
    the-semaphore))

; make-semaphore-2 constructor procedure (uses test-and-set!)
(define (make-semaphore-2 n)
  (let ((count 0)
        (cell (list false)))
    (define (add-count! k)
      (cond ((test-and-set! cell) (add-count! k))
            (else (set! count (+ count k))
                  (set-car! cell false))))
    (define (the-semaphore m)
      (cond ((and (eq? m 'acquire) (< count n)) (add-count! 1))
            ((eq? m 'acquire) (the-semaphore m))
            ((and (eq? m 'release) (> count 0)) (add-count! -1))
            ((eq? m 'release) unspecific)))
    the-semaphore))

; Part (a)
(define sema-1 (make-semaphore-1 2))
(sema-1 'acquire)
(sema-1 'acquire)
; loops in (mutex 'acquire), typed C-g to exit
(sema-1 'acquire)
(sema-1 'release)
(sema-1 'release)
; add'l releases are no-ops
(sema-1 'release)

; Part (b)
(define sema-2 (make-semaphore-2 2))
(sema-2 'acquire)
(sema-2 'acquire)
; loops in (add-count! k), typed C-g to exit
(sema-2 'acquire)
(sema-2 'release)
(sema-2 'release)
; add'l releases are no-ops
(sema-2 'release)
