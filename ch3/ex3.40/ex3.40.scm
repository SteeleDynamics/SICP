; disallow-preempt-current-thread procedure
(define disallow-preempt-current-thread
  (access disallow-preempt-current-thread
          (->environment '(runtime thread))))

; allow-preempt-current-thread procedure
(define allow-preempt-current-thread
  (access allow-preempt-current-thread
          (->environment '(runtime thread))))

; kill-thread procedure
(define (kill-thread thread)
  (let ((event (lambda () (exit-current-thread 'RIP))))
    (without-interrupts
      (lambda ()
        (case (thread-execution-state thread)
          ((STOPPED) (restart-thread thread #t event))
          ((DEAD) unspecific)
          (else (signal-thread-event thread event)))))))

; parallel-execute procedure
(define (parallel-execute . thunks)
  (let ((my-threads '()))
    (define (terminator)
      (without-interrupts
        (lambda ()
          (for-each kill-thread my-threads)
          (set! my-threads '())
          unspecific)))
    (without-interrupts
      (lambda ()
        (set! my-threads
              (map (lambda (thunk)
                     (let ((thread (create-thread #f thunk)))
                       (detach-thread thread)
                       thread))
                   thunks))
        unspecific))
    terminator))

; make-serializer constructor procedure
(define (make-serializer)
  (let ((mutex (make-thread-mutex)))
    (define (serialized f)
      (define (serialized-f . args)
        (with-thread-mutex-locked
          mutex
          (lambda ()
            (apply f args))))
      serialized-f)
    serialized))

; output serializer (IO system is not completely interlocked)
(define output-serialized (make-serializer))

; write-line procedure (serialized)
(define write-line
  (output-serialized write-line))

; display procedure (serialized)
(define display
  (output-serialized display))

; write procedure (serialized)
(define write
  (output-serialized write))

; try procedure
(define (try n)
  (parallel-execute
    (lambda ()
      (write-line 'hi)
      (let loop ((i 0))
        (if (< i 262144)
            (loop (1+ i))))
      (write-line 'gjs))
    (lambda ()
      (write-line 'there)
      (let loop ((i 0))
        (if (< i n)
            (loop (1+ i))))
      (write-line 'foo))))

; show interleaved processes/thunks
(begin (define foo (try 262141))
       (newline)
       (sleep-current-thread 1024)
       (foo))

