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
    ; (define (accept-action-procedure! proc)
    ;   (set! action-procedures (cons proc action-procedures))
    ;   (proc))
    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures)))
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

; half-adder procedure
(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

;
; Exercise 3.31:
; ##############
;
; The internal procedure accept-action-procedure! defined in make-wire specifies
; that when a new action procedure is added to a wire, the procedure is
; immediately run. Explain why this initialization is necessary. In particular,
; trace through the half-adder example in the paragraphs above and say how the
; system's response would differ if we had defined accept-action-procedure! as
;
;     (define (accept-action-procedure! proc)
;       (set! action-procedures (cons proc action-procedures)))
;
; Answer:
; #######

; The reason why the action procedure is immediately run is to update the
; signal value on the wire once it is connected to a function box. Given the
; original definiton of accept-action-procedure!, suppose the following is
; evaluated:
;
;     1 ]=> (define the-agenda (make-agenda))
;     ;Value: the-agenda
;
;     1 ]=> (define inverter-delay 2) ; set inverter-delay to 2
;     ;Value: inverter-delay
;
;     1 ]=> (define s0 (make-wire))   ; initial signal value of 0
;     ;Value: s0
;
;     1 ]=> (define s1 (make-wire))   ; initial signal value of 0
;     ;Value: s1
;
;     1 ]=> (probe 's1 s1)            ; put probe on s1
;     s1 0 New-value = 0
;     ;Unspecified return value
;
;     1 ]=> (inverter s0 s1)          ; adds action proc to agenda after delay
;     ;Value: ok
;
;     1 ]=> (propagate)               ; run simulation
;     s1 2  New-value = 1
;     ;Value: done
;
; We expect the signal value of s1 to change from 0 to 1. The only way this
; occurs is if we apply the action procedure invert-input. More specifically,
; evaluating the expression (proc) where proc has the value invert-input. This
; procedure adds the action (lambda () (set-signal! output new-value)) to the
; agenda after inverter-delay time. The procedure propagate then applies the
; action which ultimately changes the value of s1.
;
; If we change the definition of accept-action-procedure! to exclude the
; immediate procedure application (proc), the half-adder example yields the
; following results:
;

(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))

(probe 'sum sum)
(probe 'carry carry)

(half-adder input-1 input-2 sum carry)
(set-signal! input-1 1)
(propagate)

(set-signal! input-2 1)
(propagate)

;
; Omitting the (proc) expression means the output signal from the inverter
; internal to the half-adder (D) doesn't get set to the value 1. Therefore,
; the signal value for wire sum doesn't change from 0 to 1 at time 8.
;
