; memo-proc procedure
(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? true)
                 result)
          result))))

#|
 | §3.5.1 Footnote 56:
 |
 | Although stream-car and stream-cdr can be defined as procedures, cons-stream
 | must be a special form. If cons-stream were a procedure, then, according to
 | our model of evaluation, evaluating (cons-stream <a> <b>) would automatically
 | cause <b> to be evaluated, which is precisely what we do not want to happen.
 | For the same reason, delay must be a special form, though force can be an
 | ordinary procedure.
 |#

; delay procedure -- create thunk
; (define (delay expr) (lambda () expr))
; (define (delay expr) (memo-proc (lambda () expr)))
(define-syntax delay
  (syntax-rules ()
    ((delay ?expr) (memo-proc (lambda () ?expr)))))

; force procedure -- evaluate thunk
(define (force delayed-expr) (delayed-expr))

; the-empty-stream defn
(define the-empty-stream '())

; cons-stream constructor procedure
; (define (cons-stream a b) (cons a (delay b)))
(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream a b) (cons a (delay b)))))

; stream-car selector procedure
(define (stream-car stream) (car stream))

; stream-cdr selector procedure
(define (stream-cdr stream) (force (cdr stream)))

; stream-null? predicate procedure
(define (stream-null? stream) (null? stream))

; stream-ref procedure
(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

#|
 | ; stream-map procedure
 | (define (stream-map proc s)
 |   (if (stream-null? s)
 |       the-empty-stream
 |       (cons-stream (proc (stream-car s))
 |                    (stream-map proc (stream-cdr s)))))
 |#

; stream-for-each procedure
(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))
#|
 | ; display-stream procedure
 | (define (display-stream s)
 |   (stream-for-each display-line s))
 |#

; display-line procedure
(define (display-line x)
  (newline)
  (display x))

; display-stream procedure (instructor's manual, augmented)
(define display-stream
  (let ()
    (define (iter stream delim)
      (cond ((stream-null? stream) (display "]"))
            (else (display delim)
                  (display (stream-car stream))
                  (iter (stream-cdr stream) " "))))
    (lambda (stream)
      (newline)
      (display "[")
      (iter stream ""))))

; stream-enumerate-interval procedure
(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

; stream-filter procedure
(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

; stream-map procedure
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
        (apply proc (map stream-car argstreams))
        (apply stream-map
          (cons proc (map stream-cdr argstreams))))))

#|
 | Exercise 3.51
 |
 | In order to take a closer look at delayed evaluation, we will use the
 | following procedure, which simply returns its argument after printing it:
 |#

(define (show x)
  (display-line x)
  x)

#|
 | What does the interpreter print in response to evaluating each expression in
 | the following sequence?
 |#

(define x (stream-map show (stream-enumerate-interval 0 10)))
(stream-ref x 5)
(stream-ref x 7)
