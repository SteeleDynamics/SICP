#|
 | wire mutable object procedures
 |#

; (helper) call-each procedure
(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin
        ((car procedures))
        (call-each (cdr procedures)))))

; make-wire constructor procedure
(define (make-wire)
  (let ((signal-value 0) (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation -- WIRE" m))))
    dispatch))

; get-signal selector procedure
(define (get-signal wire)
  (wire 'get-signal))

; set-signal! mutator procedure
(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))

; add-action! mutator procedure
(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

; probe procedure
(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (newline)
                 (display name)
                 (display " ")
                 (display (current-time the-agenda))
                 (display "  New-value = ")
                 (display (get-signal wire)))))

#|
 | queue mutable object procedures
 |#

; (helper) front-ptr selector procedure
(define (front-ptr queue) (car queue))

; (helper) rear-ptr selector procedure
(define (rear-ptr queue) (cdr queue))

; (helper) set-front-ptr! mutator procedure
(define (set-front-ptr! queue item) (set-car! queue item))

; (helper) set-rear-ptr! mutator procedure
(define (set-rear-ptr! queue item) (set-cdr! queue item))

; make-queue constructor procedure
(define (make-queue) (cons '() '()))

; empty-queue? predicate procedure
(define (empty-queue? queue) (null? (front-ptr queue)))

; front-queue selector procedure
(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

; insert-queue! mutator procedure
(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

; delete-queue! mutator procedure
(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue)))

#|
 | agenda mutable object procedures
 | (a priority queue impl, not a binary heap)
 |#

; make-time-segment constructor procedure
(define (make-time-segment time queue)
  (cons time queue))

; segment-time selector procedure
(define (segment-time s) (car s))

; segment-queue selector procedure
(define (segment-queue s) (cdr s))

; make-agenda constructor procedure
(define (make-agenda) (list 0))

; current-time selector procedure
(define (current-time agenda) (car agenda))

; set-current-time! mutator procedure
(define (set-current-time! agenda time)
  (set-car! agenda time))

; segments selector procedure
(define (segments agenda) (cdr agenda))

; set-segments! mutator procedure
(define (set-segments! agenda segments)
  (set-cdr! agenda segments))

; first-segment selector procedure
(define (first-segment agenda) (car (segments agenda)))

; rest-segments selector procedure
(define (rest-segments agenda) (cdr (segments agenda)))

; empty-agenda? predicate procedure
(define (empty-agenda? agenda)
  (null? (segments agenda)))

; add-to-agenda! mutator procedure
(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
        (insert-queue! (segment-queue (car segments))
                       action)
        (let ((rest (cdr segments)))
          (if (belongs-before? rest)
              (set-cdr!
               segments
               (cons (make-new-time-segment time action)
                     (cdr segments)))
              (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments!
         agenda
         (cons (make-new-time-segment time action)
               segments))
        (add-to-segments! segments))))

; remove-first-agenda-item! mutator procedure
(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments! agenda (rest-segments agenda)))))

; first-agenda-item selector procedure
(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty -- FIRST-AGENDA-ITEM")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))

#|
 | primitive function boxes (logic gates) procedures
 |#

; (helper) after-delay procedure
(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

; (helper) invalid-signal? predicate procedure
(define (invalid-signal? s)
  (not (or (= s 0)
           (= s 1))))

; (helper) logical-not procedure
(define (logical-not s)
  (cond ((invalid-signal? s) (error "Invalid signal -- LOGICAL-NOT" s))
        ((= s 0) 1)
        ((= s 1) 0)))

; (helper) logical-and procedure
(define (logical-and s1 s2)
  (cond ((invalid-signal? s1) (error "Invalid signal s1 -- LOGICAL-AND" s1))
        ((invalid-signal? s2) (error "Invalid signal s2 -- LOGICAL-AND" s2))
        ((and (= s1 1) (= s2 1)) 1)
        (else 0)))

; (helper) logical-or procedure
(define (logical-or s1 s2)
  (cond ((invalid-signal? s1) (error "Invalid signal s1 -- LOGICAL-OR" s1))
        ((invalid-signal? s2) (error "Invalid signal s2 -- LOGICAL-OR" s2))
        ((or (= s1 1) (= s2 1)) 1)
        (else 0)))

; inverter constructor procedure
(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

; and-gate constructor procedure
(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

; or-gate constructor procedure
(define (or-gate o1 o2 output)
  (define (or-action-procedure)
    (let ((new-value
           (logical-or (get-signal o1) (get-signal o2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! o1 or-action-procedure)
  (add-action! o2 or-action-procedure)
  'ok)

#|
 | driver for digital circuits simulator
 |#

; propagate procedure
(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

#|
 | add'l digital circuits
 |#

;
;         half-adder
;      ┌──────────────────────────────────────────────────┐
;      │        ┌────────┐                                │
;  A ──┼─────•──┤        │         D          ┌────────┐  │
;      │     │  │or-gate ├────────────────────┤        │  │
;      │  ┌──┼──┤        │                 E  │and-gate├──┼── S
;      │  │  │  └────────┘                 ┌──┤        │  │
;      │  │  │                 ┌────────┐  │  └────────┘  │
;      │  │  │  ┌────────┐  ┌──┤inverter├──┘              │
;      │  │  └──┤        │  │  └────────┘                 │
;      │  │     │and-gate├──•─────────────────────────────┼── C
;  B ──┼──•─────┤        │                                │
;      │        └────────┘                                │
;      └──────────────────────────────────────────────────┘
;
; (define half-adder-delay
;   (max (+ or-gate-delay and-gate-delay)
;        (+ (* 2 and-gate-delay) inverter-delay)))
;

; half-adder procedure
(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

;
;           full-adder
;        ┌──────────────────────────────────────────────────┐
;        │                    ┌────────┐                    │
;    A ──┼────────────────────┤ half-  ├────────────────────┼── SUM
;        │     ┌────────┐     │ adder  │     ┌────────┐     │
;    B ──┼─────┤ half-  ├─────┤        ├─────┤        │     │
;        │     │ adder  │     └────────┘     │or-gate ├─────┼── C-out
; C-in ──┼─────┤        ├────────────────────┤        │     │
;        │     └────────┘                    └────────┘     │
;        └──────────────────────────────────────────────────┘
;
; (define full-adder-delay
;   (+ (* 2 half-adder-delay)
;      or-gate-delay))
;

; full-adder procedure
(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

;
;           ripple-carry-adder
;        ┌─────────────────────────────────────────────┐
;        │     ┌────────┐                              │
;        │C0=0 ┤ full-  ├──────────────────────────────┼── S1
;   A1 ──┼─────┤ adder  │                              │
;   B1 ──┼─────┤        ├──┐                           │
;        │     └────────┘  │ C1                        │
;        │       ┌─────────┘                           │
;        │       │  ┌────────┐                         │
;        │       └──┤ full-  ├─────────────────────────┼── S2
;   A2 ──┼──────────┤ adder  │                         │
;   B2 ──┼──────────┤        ├──┐                      │
;        │          └────────┘  │ C2                   │
;        │                 ─────┘                      │
;                               .
;                               .
;                               .
;        │                           ┌─────            │
;        │                        Cn │  ┌────────┐     │
;        │                           └──┤ full-  ├─────┼── Sn
;   An ──┼──────────────────────────────┤ adder  │     │
;   Bn ──┼──────────────────────────────┤        ├─────┼── C
;        │                              └────────┘     │
;        └─────────────────────────────────────────────┘
;
;     (define ripple-carry-adder-delay
;       (* n full-adder-delay))
;
;     ripple-carry-adder-delay
; ==> (* n full-adder-delay)
; ==> (* n (+ (* 2 half-adder-delay) or-gate-delay))
; ==> (* n (+ (* 2 (max (+ or-gate-delay and-gate-delay)
;                       (+ (* 2 and-gate-delay) inverter-delay)))
;             or-gate-delay))
;

; ripple-carry-adder procedure
(define (ripple-carry-adder as bs ss c-out)
  (define (iter as bs ss c-in c-out)
    (cond ((not (null? as))
             (full-adder (car as) (car bs) c-in (car ss) c-out)
             (iter (cdr as) (cdr bs) (cdr ss) (make-wire) c-in))
          (else 'ok)))
  (iter as bs ss (make-wire) c-out))

#|
 | unit tests
 |#

; define the-agenda and delays for logic gates
(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

; define 4-bit numbers As, Bs, Ss; and carry-bit C
(define As (list (make-wire) (make-wire) (make-wire) (make-wire)))
(define Bs (list (make-wire) (make-wire) (make-wire) (make-wire)))
(define Ss (list (make-wire) (make-wire) (make-wire) (make-wire)))
(define C (make-wire))

; wire up 4-bit ripple-carry-adder
(ripple-carry-adder As Bs Ss C)

; put probes on Ss
(probe 's1 (cadddr Ss))
(probe 's2 (caddr Ss))
(probe 's3 (cadr Ss))
(probe 's4 (car Ss))
(probe 'c C)

; As ==> 6 decimal or 0110 binary
(set-signal! (caddr As) 1)
(set-signal! (cadr As) 1)

; Bs ==> 7 decimal or 0111 binary
(set-signal! (cadddr Bs) 1)
(set-signal! (caddr Bs) 1)
(set-signal! (cadr Bs) 1)

; run simulation ==> resulting in 1101 binary
(propagate)
