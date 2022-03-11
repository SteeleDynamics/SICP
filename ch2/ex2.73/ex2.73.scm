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

; create instance of operation-table
(define operation-table (make-table))

; get and put procedures
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

; deriv procedure
(define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp) (if (same-variable? exp var) 1 0))
         (else ((get 'deriv (operator exp)) (operands exp)
                                            var))))

; operator selector procedure
(define (operator exp) (car exp))

; operand selector procedure
(define (operands exp) (cdr exp))

; variable? predicate procedure
(define (variable? x) (symbol? x))

; same-variable? predicate procedure
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

; make-sum constructor procedure
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

; =number? predicate procedure
(define (=number? exp num)
  (and (number? exp) (= exp num)))

; make-product constructor procedure
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

; make-exponentiation constructor procedure
(define (make-exponentiation base exponent)
  (cond ((= exponent 0) 1)
        ((= exponent 1) base)
        ((number? base) (expt base exponent))
        (else (list base exponent))))

;
; Part (a)
; ########
; What was done was that we are now dispatching on the expr's operator symbol to
; apply the correct deriv procedure to the expr. We cannot include number? and
; same-variable? predicates because they do not recursively call deriv.

;
; Part (b)
; ########
;
; install-sum-deriv procedure
(define (install-sum-deriv)
  ; sum-deriv procedure to dispatch
  (define (sum-deriv exp var)
    (let ((e1 (car exp)) (e2 (cadr exp)))
      (make-sum (deriv e1 var) (deriv e2 var))))

  ; add deriv procedure to operation-table
  (put 'deriv '+ sum-deriv)
  'done)

; install-product-deriv procedure
(define (install-product-deriv)
  ; product-deriv procedure to dispatch
  (define (product-deriv exp var)
    (let ((e1 (car exp)) (e2 (cadr exp)))
      (make-sum
        (make-product e1 (deriv e2 var))
        (make-product (deriv e1 var) e2))))

  ; add deriv procedure to operation-table
  (put 'deriv '* product-deriv)
  'done)

;
; Part (c)
; ########
;
; install-exponential-deriv procedure
(define (install-exponential-deriv)
  ; exponential-deriv procedure to dispatch
  (define (exponential-deriv exp var)
    (let ((e (car exp)) (k (cadr exp)))
      (if (= k 0)
          0
          (make-product
            (make-product
              k
              (make-exponentiation e (- k 1)))
            (deriv e var)))))

  ; add deriv procedure to operation-table
  (put 'deriv '** exponential-deriv)
  'done)

; installing deriv procedures
(install-sum-deriv)
(install-product-deriv)
(install-exponential-deriv)

; unit tests
(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)
(deriv '(* (* x y) (+ x 3)) 'x)

; expression: ax^3 + bx^2 + cx^1 + dx^0
(deriv '(+ (* a (** x 3))
           (+ (* b (** x 2))
              (+ (* c (** x 1))
                 (* d (** x 0)))))
       'x)

; expression: (ax + b)^2
(deriv '(** (+ (* a x) b) 2) 'x)

;
; Part (d)
; ########
; If the table indexing were swapped, and the portion of code doing the dispatch
; in the deriv procedure was modified accordingly:
;
;             ((get (operator exp) 'deriv) (operands exp) var)
;
; Then no additional changes are required to make the derivative system work.
; The values (procedures) resulting from lookup evaluation still get applied
; to the values of (operators exp) and var in the same way as before.
;
