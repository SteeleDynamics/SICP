;
;  (define x (make-element key))
;
;      ┌─┬─┐  ┌─┬─┐                           ┌─┬───┬─┐
;  x ─>│╱│•┼─>│•│╱│         redrawn as    x ─>│╱│key│╱│
;      └─┴─┘  └┼┴─┘           ====>           └─┴───┴─┘
;              V
;            ┌───┐
;            │key│
;            └───┘
;
'()

; make-element constructor procedure
(define (make-element key)
  (cons '() (cons key '())))

; prev-element selector procedure
(define (prev-element elt)
  (car elt))

; key-element selector procedure
(define (key-element elt)
  (cadr elt))

; next-element selector procedure
(define (next-element elt)
  (cddr elt))

; set-prev-element! mutator procedure
(define (set-prev-element! elt value)
  (set-car! elt value))

; set-next-element! mutator procedure
(define (set-next-element! elt value)
  (set-cdr! (cdr elt) value))

;
; deque impl'ed as doubly-linked list of elemets with front and rear pointers
;
;
;      ┌─┬─┐
;  d ─>│•│•┼───────────────────────┐
;      └┼┴─┘                       │
;       │ front-ptr                │ rear-ptr
;       V                          V
;      ┌─┬─┬─┐  ┌─┬─┬─┐  ┌─┬─┬─┐  ┌─┬─┬─┐
;      │╱│w│•┼─>│•│x│•┼─>│•│y│•┼─>│•│z│╱│
;      └─┴─┴─┘  └┼┴─┴─┘  └┼┴─┴─┘  └┼┴─┴─┘
;           Λ    │   Λ    │   Λ    │
;           └────┘   └────┘   └────┘
;
;
'()

; make-deque constructor procedure
(define (make-deque)
  (cons '() '()))

; front-ptr selector procedure
(define (front-ptr deque)
  (car deque))

; rear-ptr selector procedure
(define (rear-ptr deque)
  (cdr deque))

; set-front-ptr! mutator procedure
(define (set-front-ptr! deque elt)
  (set-car! deque elt))

; set-rear-ptr! mutator procedure
(define (set-rear-ptr! deque elt)
  (set-cdr! deque elt))

; empty-deque? predicate procedure
(define (empty-deque? deque)
  (and (null? (front-ptr deque))
       (null? (rear-ptr deque))))

; front-deque selector procedure
(define (front-deque deque)
  (if (empty-deque? deque)
      (error "FRONT called with an empty deque" deque)
      (key-element (front-ptr deque))))

; rear-deque selector procedure
(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "REAR called with an empty deque" deque)
      (key-element (rear-ptr deque))))

; front-insert-deque! mutator procedure
(define (front-insert-deque! deque key)
  (let ((new-elt (make-element key)))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-elt)
           (set-rear-ptr! deque new-elt)
           deque)
          (else
           (set-prev-element! (front-ptr deque) new-elt)
           (set-next-element! new-elt (front-ptr deque))
           (set-front-ptr! deque new-elt)
           deque))))

; rear-insert-deque! mutator procedure
(define (rear-insert-deque! deque key)
  (let ((new-elt (make-element key)))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-elt)
           (set-rear-ptr! deque new-elt)
           deque)
          (else
           (set-next-element! (rear-ptr deque) new-elt)
           (set-prev-element! new-elt (rear-ptr deque))
           (set-rear-ptr! deque new-elt)
           deque))))

; front-delete-deque! mutator procedure
(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "FRONT-DELETE! called with an empty deque" deque))
        ((null? (next-element (front-ptr deque)))
         (set-front-ptr! deque '())
         (set-rear-ptr! deque '())
         deque)
        (else
         (set-front-ptr! deque (next-element (front-ptr deque)))
         (set-prev-element! (front-ptr deque) '())
         deque)))

; rear-delete-deque! mutator procedure
(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "REAR-DELETE! called with an empty deque" deque))
        ((null? (prev-element (rear-ptr deque)))
         (set-rear-ptr! deque '())
         (set-front-ptr! deque '())
         deque)
        (else
         (set-rear-ptr! deque (prev-element (rear-ptr deque)))
         (set-next-element! (rear-ptr deque) '())
         deque)))

; print-deque procedure
(define (print-deque deque)
  (define (iter elt)
    (cond ((null? elt)
           (newline)
           'done)
          (else
           (display (key-element elt))
           (display " ")
           (iter (next-element elt)))))
  (newline)
  (iter (front-ptr deque)))

; unit-tests
(define d (make-deque))
(print-deque (front-insert-deque! d 'x))
(print-deque (rear-insert-deque! d 'y))
(print-deque (front-insert-deque! d 'w))
(print-deque (rear-insert-deque! d 'z))
(front-deque d)
(rear-deque d)
(print-deque (front-delete-deque! d))
(print-deque (rear-delete-deque! d))
(print-deque (front-delete-deque! d))
(print-deque (rear-delete-deque! d))
