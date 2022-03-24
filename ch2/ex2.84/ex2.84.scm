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

; partial-order-leq? predicate procedure
(define (partial-order-leq? a b)
  (define (height a)
    (cond ((eq? a 'integer) 0)
          ((eq? a 'rational) 1)
          ((eq? a 'real) 2)
          ((eq? a 'complex) 3)))
  (<= (height a) (height b)))

; partial-order-eq? predicate procedure
(define (partial-order-eq? a b)
  (and (partial-order-leq? a b)
       (partial-order-leq? b a)))

; apply-generic procedure
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (args1 (car args))
                    (args2 (cadr args)))
                (cond ((partial-order-eq? type1 type2)
                       (error "No method for these types" (list op type-tags)))
                      ((partial-order-leq? type1 type2)
                       (apply-generic op (apply-generic 'raise args1) args2))
                      (else
                       (apply-generic op args1 (apply-generic 'raise args2)))))
              (error "No method for these types" (list op type-tags)))))))

; install-integer-pkg procedure
(define (install-integer-pkg)
  ; internal procedures
  (define (raise-int x)
    (cons x 1))
  (define (raise x)
    (attach-tag 'rational (raise-int x)))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'integer x))
  (put 'add '(integer integer) (lambda (x y) (tag (+ x y))))
  (put 'raise '(integer) raise)
  (put 'make 'integer (lambda (x) (tag x)))
  'done)

; install-rational-pkg procedure
(define (install-rational-pkg)
  ; internal procedures
  (define (raise-rat x)
    (/ (* (car x) 1.0) (cdr x)))
  (define (raise x)
    (attach-tag 'real (raise-rat x)))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational) (lambda (x y) (tag (add-rat x y))))
  (put 'raise '(rational) raise)
  (put 'make 'rational (lambda (x y) (tag (cons x y))))
  'done)

; install-real-pkg procedure
(define (install-real-pkg)
  ; internal procedures
  (define (raise-real x)
    (cons x 0.0))
  (define (raise x)
    (attach-tag 'complex (raise-real x)))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'real x))
  (put 'add '(real real) (lambda (x y) (tag (+ x y))))
  (put 'raise '(real) raise)
  (put 'make 'real (lambda (x) (tag x)))
  'done)

; install-complex-pkg procedure
(define (install-complex-pkg)
  ; internal procedures
  (define (make-from-real-imag x y) (cons x y))
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'complex x))
  (put 'add '(complex complex) (lambda (x y) (tag (add-complex x y))))
  (put 'make 'complex (lambda (x y) (tag (make-from-real-imag x y))))
  'done)

; unit tests
(install-integer-pkg)
(install-rational-pkg)
(install-real-pkg)
(install-complex-pkg)

(define x ((get 'make 'rational) 2 3))
(define y ((get 'make 'complex) 4.2 6.4))
(apply-generic 'add x y)

