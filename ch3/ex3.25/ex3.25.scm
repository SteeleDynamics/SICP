; make-table constructor procedure
(define (make-table)
  ; local-table internal state
  (let ((local-table (list '*table*)))
    ; assoc selector prcedure
    (define (assoc key records)
      (cond ((null? records) false)
            ((equal? key (caar records)) (car records))
            (else (assoc key (cdr records)))))
    ; lookup selector procedure
    (define (lookup . args)
      (define (iter tbl args)
        (let ((sub (assoc (car args) (cdr tbl))))
          (cond ((not sub) false)
                ((null? (cdr args)) (cdr sub))
                (else (iter sub (cdr args))))))
      (iter local-table args))
    ; insert! mutator procedure
    (define (insert! . args)
      (define (iter! tbl args)
        (let ((sub (assoc (car args) (cdr tbl))))
          (cond ((and (not sub) (null? (cddr args)))
                 (set-cdr! tbl (cons (cons (car args) (cadr args)) (cdr tbl))))
                ((not sub)
                 (set-cdr! tbl (cons (list (car args)) (cdr tbl)))
                 (iter! (cadr tbl) (cdr args)))
                ((null? (cddr args))
                 (set-cdr! sub (cadr args)))
                (else
                 (iter! sub (cdr args))))))
      (iter! local-table args)
      'ok)
    ; dispatch message-passing procedure
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    ; return dispatch procedure value as table object
    dispatch))

; unit-tests
(define tbl1 (make-table))
((tbl1 'insert-proc!) 'foo 0)
((tbl1 'insert-proc!) 'bar 1)
((tbl1 'insert-proc!) 'baz 2)
((tbl1 'insert-proc!) 'qux 3)
((tbl1 'lookup-proc) 'foo)
((tbl1 'lookup-proc) 'bar)
((tbl1 'lookup-proc) 'baz)
((tbl1 'lookup-proc) 'qux)
((tbl1 'lookup-proc) 'quux)

(define tbl2 (make-table))
((tbl2 'insert-proc!) 'foo 'foo 'foo 0)
((tbl2 'insert-proc!) 'foo 'foo 'bar 1)
((tbl2 'insert-proc!) 'foo 'bar 'foo 2)
((tbl2 'insert-proc!) 'foo 'bar 'bar 3)
((tbl2 'insert-proc!) 'bar 'foo 'foo 4)
((tbl2 'insert-proc!) 'bar 'foo 'bar 5)
((tbl2 'insert-proc!) 'bar 'bar 'foo 6)
((tbl2 'insert-proc!) 'bar 'bar 'bar 7)
((tbl2 'lookup-proc) 'foo 'foo 'foo)
((tbl2 'lookup-proc) 'foo 'foo 'bar)
((tbl2 'lookup-proc) 'foo 'bar 'foo)
((tbl2 'lookup-proc) 'foo 'bar 'bar)
((tbl2 'lookup-proc) 'bar 'foo 'foo)
((tbl2 'lookup-proc) 'bar 'foo 'bar)
((tbl2 'lookup-proc) 'bar 'bar 'foo)
((tbl2 'lookup-proc) 'bar 'bar 'bar)
((tbl2 'lookup-proc) 'foo 'bar 'baz)