; assoc procedure
(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

; make-table procedure
(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

; instantiate operation-table, get/put procedures
(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

; attach-tag procedure
(define (attach-tag t contents)
  (cons t contents))

; type-tag selector procedure
(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        (else (error "Bad tagged datum -- TYPE-TAG" datum))))

; contents selector procedure
(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        (else (error "Bad tagged datum -- CONTENTS" datum))))

; apply-generic procedure
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
            "No method for these types -- APPLY-GENERIC"
            (list op type-tags))))))

; install-integer-pkg procedure
(define (install-integer-pkg)
  ; internal procedures
  (define (make-rat a b)
    (cons a b))
  (define (raise x)
    (attach-tag 'rational (make-rat x 1)))

  ;; interface to the rest of the system
  (put 'raise '(integer) raise)
  (put 'make 'integer (lambda (x) (attach-tag 'integer x)))
  'done)

; install-rational-pkg procedure
(define (install-rational-pkg)
  ; internal procedures
  (define (make-real x)
    (/ (* (car x) 1.0) (cdr x)))
  (define (raise x)
    (attach-tag 'real (make-real x)))

  ;; interface to the rest of the system
  (put 'raise '(rational) raise)
  'done)

; install-real-pkg procedure
(define (install-real-pkg)
  ; internal procedures
  (define (make-complex x)
    (cons x 0.0))
  (define (raise x)
    (attach-tag 'complex (make-complex x)))

  ;; interface to the rest of the system
  (put 'raise '(real) raise)
  'done)

; install-complex-pkg procedure
(define (install-complex-pkg)
  ; Unimplemented...
  'done)

; unit tests
(install-integer-pkg)
(install-rational-pkg)
(install-real-pkg)
(install-complex-pkg)

(define a0 ((get 'make 'integer) 2))
(define a1 (apply-generic 'raise a0))
(define a2 (apply-generic 'raise a1))
(define a3 (apply-generic 'raise a2))
a0
a1
a2
a3
