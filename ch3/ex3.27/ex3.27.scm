; assoc procedure
(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

; make-table constructor procedure
(define (make-table)
  (list '*table*))

; lookup selector procedure
(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        false)))

; insert! mutator procedure
(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value) (cdr table)))))
  'ok)

; fib procedure
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

; memoize procedure
(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((previously-computed-result (lookup x table)))
        (or previously-computed-result
            (let ((result (f x)))
              (insert! x result table)
              result))))))

; memo-fib procedure
(define memo-fib
  (memoize (lambda (n)
             (cond ((= n 0) 0)
                   ((= n 1) 1)
                   (else (+ (memo-fib (- n 1))
                            (memo-fib (- n 2))))))))

; receiver procedure
(define (receiver run-time gc-time real-time)
    (newline)
    (write (internal-time/ticks->seconds run-time))
    (write-char #\space)
    (write (internal-time/ticks->seconds gc-time))
    (write-char #\space)
    (write (internal-time/ticks->seconds real-time)))

; (with-timings thunk receiver) --> evaluate (fib 31) and (memo-fib 31)
'()
(with-timings (lambda () (fib 31)) receiver)
(with-timings (lambda () (memo-fib 31)) receiver)

;
;
; Exercise 3.27
; #############
;
; Draw an environment diagram to analyze the computation of (memo-fib 3).
; Explain why memo-fib computes the nth Fibonacci number in a number of steps
; proportional to n. Would the scheme still work if we had simply defined
; memo-fib to be (memoize fib)?
;
; Instructor's Manual:
; ####################
;
; The intent of exercise 3.27 is to show the effect of memoization on the
; number of calls to fib. The number of steps should be measured in terms of
; the number of calls to fib, not taking into account the time to execute the
; table operations. (That is, the table operations should be assumed to take a
; constant number of steps, even though this is not true of the operations we
; implemented in this section. If students worry that this is not a realistic
; assumption, you can tell them about hash tables.) Moreover, for the purpose
; of this exercise the table operations should be considered primitive, so that
; calls to insert! and lookup needn't be analyzed in the environment diagram.
; (If you include the analysis of the actual insert! and lookup calls, you get
; an environment diagram with over 50 frames. This complexity obscures the point
; being made in this exercise.)
;
;
;           ┌────────────────────────────────────────────────────────────────╱
;           │ assoc: ──────────┐  lookup: ───────────────────────────────────╲
; global -->│ make-table: ─┐   │  insert!: ─┐                                ╱
;  env      └──────────────┼───┼────────────┼────────────────────────────────╲
;                ┌─────────┘ Λ │ Λ          │ Λ
;                V           │ │ │          │ │
;  ┌────────────ꙨꙨ───────────┘ │ │          │ │
;  V                           │ │          │ │
; params: -                    │ │          │ │
; body: (list '*table*)        │ │          │ │
;           ┌──────────────────┘ │          │ │
;           │ ┌──────────────────┘          │ │
;           │ │                  ┌──────────┘ │
;           │ │                  V            │
;           │ │   ┌─────────────ꙨꙨ────────────┘
;           │ │   V
;           │ │ params: key, value, table
;           │ │ body: (let ((record (assoc key (cdr table))))
;           │ │         (if record
;           │ │             (set-cdr! record value)
;        ┌──┘ │             (set-cdr! table
;        V    │                       (cons (cons key value) (cdr table)))))
;  ┌────ꙨꙨ────┘       'ok
;  V
; params: key, records
; body: (cond ((null? records) false)
;             ((equal? key (caar records)) (car records))
;             (else (assoc key (cdr records))))
;
;
; ╱───────────────────────────────────────────────────┐
; ╲────┐    memoize: ──────────────────────────┐      │
; ╱    │    memo-fib: ─┐                       │      │
; ╲────┼───────────────┼───────────────────────┼──────┘
;      │ Λ    ┌────────┘ Λ         ┌───────────┘ Λ
;      │ │    │ ┌────────┘         V             │
;      │ │    │ │   ┌─────────────ꙨꙨ─────────────┘
;      │ │    │ │   V
;      │ │    │ │ params: f
;      │ │    │ │ body: (let ((table (make-table)))
;      │ │    │ │         (lambda (x)
;      │ │    │ │           (let ((previously-computed-result (lookup x table)))
;      │ │    │ │             (or previously-computed-result
;      │ │    │ │                 (let ((result (f x)))
;      │ │    V │                   (insert! x result table)
;      │ │  ┌ꙨꙨ─┘                   result)))))
;      │ │  V
;      │ │  params: -
;      │ │  body: (memoize (lambda (n)
;      │ │                   (cond ((= n 0) 0)
;      V │                         ((= n 1) 1)
;   ┌─ꙨꙨ─┘                         (else (+ (memo-fib (- n 1))
;   V                                       (memo-fib (- n 2)))))))
;  params: key, table
;  body: (let ((record (assoc key (cdr table))))
;          (if record
;              (cdr record)
;              false))
;
