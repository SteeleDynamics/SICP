; nil defn
(define nil '())

; fold-left procedure
(define (fold-left op init seq)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter init seq))

; fold-right procedure
(define (fold-right op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (fold-right op init (cdr seq)))))

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

; instantiate coercion-table, get-coercion/put-coercion procedures
(define coercion-table (make-table))
(define get-coercion (coercion-table 'lookup-proc))
(define put-coercion (coercion-table 'insert-proc!))

; attach-tag constructor procedure
(define (attach-tag type-tag contents)
  (cons type-tag contents))

; type-tag selector procedure
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))

; contents selector procedure
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

; ; apply-generic procedure
; (define (apply-generic op . args)
;   (let ((type-tags (map type-tag args)))
;     (let ((proc (get op type-tags)))
;       (if proc
;           (apply proc (map contents args))
;           (error
;             "No method for these types -- APPLY-GENERIC"
;             (list op type-tags))))))

; eq-types? predicate procedure
(define (eq-types? ts)
  (let ((t (car ts)))
    (fold-left (lambda (acc x) (and acc (eq? x t))) true ts)))

; get-coercion-col procedure
(define (get-coercion-col t ts)
  (map (lambda (x)
         (if (eq? x t)
             (lambda (x) x)
             (get-coercion x t)))
         ts))

; coercions-exist? predicate procedure
(define (coercions-exist? col)
  (fold-right (lambda (x acc) (and x acc)) true col))

; coerce-args-rec procedure
(define (coerce-args-rec col args)
  (if (null? args)
      nil
      (cons ((car col) (car args))
            (coerce-args-rec (cdr col) (cdr args)))))

; coerce-args procedure
(define (coerce-args ts args)
  (define (iter tl ts)
    (if (null? tl)
        false
        (let ((col (get-coercion-col (car tl) ts)))
          (if (coercions-exist? col)
              (coerce-args-rec col args)
              (iter (cdr tl) ts)))))
  (iter ts ts))

; apply-generic procedure
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (not (eq-types? type-tags))
              (apply apply-generic (cons op (coerce-args type-tags args)))
              (error
                "No method for these types -- APPLY-GENERIC"
                (list op type-tags)))))))

; unit tests
(define x (list 'B 2))
(define y (list 'A 3))
(put-coercion 'A 'B (lambda (x) (list 'B (cadr x))))
(put 'foo '(B B) (lambda (x y) (+ (car x) (car y))))
(apply-generic 'foo x x)
(apply-generic 'foo x y)
(apply-generic 'foo y x)
(apply-generic 'foo y y)
