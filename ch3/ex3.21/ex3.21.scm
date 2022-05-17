(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))
(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

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

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue))) 

(define q1 (make-queue))
(insert-queue! q1 'a)
(insert-queue! q1 'b)
(delete-queue! q1)
(delete-queue! q1)

;
; Eva Lu Ator is talking about how the Lisp interpreter REPL prints lists. A
; regular list in Lisp is a pointer. So when a list pointer is given to the REPL
; for evaluation, the Lisp printer pretty-prints it. For our representation of a
; queue, it is a pair of pointers. Each pointer is itself a list. Moreover, the
; rear-ptr is a cons of the last inserted item and nil.
;
; So when the front-ptr is cons'ed with rear-ptr, the Lisp printer displays the
; queue as a list of two items:
;
; 1. a list of the queue contents, and
; 2. the last item inserted into the queue.
;
; The last item of #1 and item #2 are aliases. They represent the same item.
;

; print-queue procedure
(define (print-queue q)
  (define (iter ptr)
    (cond ((null? ptr)
            newline
            'done)
          (else
            (display (car ptr))
            (display " ")
            (iter (cdr ptr)))))
  (newline)
  (iter (front-ptr q)))

; pretty-print queue
(define q2 (make-queue))
(print-queue (insert-queue! q2 'a))
(print-queue (insert-queue! q2 'b))
(print-queue (delete-queue! q2))
(print-queue (delete-queue! q2))
