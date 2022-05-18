; make-table constructor procedure
(define (make-table same-key?)
  ; local-table internal state
  (let ((local-table (list '*table*)))
    ; assoc selector procedure (uses same-key? instead of equal?)
    (define (assoc key records)
      (cond ((null? records) false)
            ((same-key? key (caar records)) (car records))
            (else (assoc key (cdr records)))))
    ; lookup selector procedure
    (define (lookup key)
      (let ((record (assoc key (cdr local-table))))
        (if record
            (cdr record)
            false)))
    ; insert! mutator procedure
    (define (insert! key value)
      (let ((record (assoc key (cdr local-table))))
        (if record
            (set-cdr! record value)
            (set-cdr! local-table
                      (cons (cons key value) (cdr local-table)))))
      'ok)
    ; dispatch message-passing procedure (represents table object)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

; unit-tests
(define (same-key? k1 k2)
  (let ((tol 0.5) (diff (- k1 k2)))
    (and (>= diff (- tol))
         (< diff tol))))

(define tbl (make-table same-key?))
((tbl 'insert-proc!) 0.42 'foo)
((tbl 'insert-proc!) 0.95 'bar)
((tbl 'insert-proc!) 1.88 'baz)
((tbl 'insert-proc!) 3.14 'foo)
((tbl 'insert-proc!) 3.27 'qux)
((tbl 'lookup-proc) 0)
((tbl 'lookup-proc) 1)
((tbl 'lookup-proc) 2)
((tbl 'lookup-proc) 3)
((tbl 'lookup-proc) 4)
