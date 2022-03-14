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

; operation-table defn (instantiation)
(define operation-table (make-table))

; get and put procedures
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

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

; apply-generic procedure
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
            "No method for these types -- APPLY-GENERIC"
            (list op type-tags))))))

; install-unordered-list-pkg procedure
(define (install-unordered-list-pkg)
  ; internal procedures
  (define (make-set elements) elements)
  (define (element-of-set? x set)
    (cond ((null? set) false)
          ((equal? x (car set)) true)
          (else (element-of-set? x (cdr set)))))
  (define (adjoin-set x set)
    (if (element-of-set? x set)
        set
        (cons x set)))
  (define (intersection-set set1 set2)
    (cond ((or (null? set1) (null? set2)) '())
          ((element-of-set? (car set1) set2)
           (cons (car set1)
                 (intersection-set (cdr set1) set2)))
          (else (intersection-set (cdr set1) set2))))
  (define (union-set set1 set2)
    (cond ((null? set1) set2)
          ((not (element-of-set? (car set1) set2))
           (cons (car set1)
                 (union-set (cdr set1) set2)))
          (else (union-set (cdr set1) set2))))
  ; record constructor and selectors - Parts (a) and (b)
  (define (mak-record name salary address)
    (list name salary address))
  (define (sel-name record)
    (car record))
  (define (sel-salary record)
    (cadr record))
  (define (sel-address record)
    (caddr record))
  ; get-record procedure - Part (a)
  (define (get-record name file)
    (cond ((null? file) false)
          ((eq? name (sel-name (car file)))
           (car file))
          (else (get-record name (cdr file)))))
  ; get-salary procedure - Part (b)
  (define (get-salary record)
    (sel-salary record))
  ; interface to the rest of the system
  (put 'make-set 'unordered-list make-set)
  (put 'element-of-set? 'unordered-list element-of-set?)
  (put 'adjoin-set 'unordered-list adjoin-set)
  (put 'intersection-set '(unordered-list unordered-list) intersection-set)
  (put 'union-set '(unordered-list unordered-list) union-set)
  ; add get-record/salary entries to operation-table - Parts (a) and (b)
  (put 'get-record 'unordered-list get-record)
  (put 'get-salary 'unordered-list get-salary)
  'done)

; install-ordered-list-pkg procedure
(define (install-ordered-list-pkg)
  ; internal procedures
  (define (make-set elements) elements)
  (define (element-of-set? x set)
    (cond ((null? set) false)
          ((= x (car set)) true)
          ((< x (car set)) false)
          (else (element-of-set? x (cdr set)))))
  (define (adjoin-set x set)
    (cond ((null? set) (cons x set))
          ((< x (car set)) (cons x set))
          ((= x (car set)) set)
          (else (cons (car set) (adjoin-set x (cdr set))))))
  (define (intersection-set set1 set2)
    (if (or (null? set1) (null? set2))
        '()
        (cond ((< (car set1) (car set2))
               (intersection-set (cdr set1) set2))
              ((= (car set1) (car set2))
               (cons (car set1) (intersection-set (cdr set1) (cdr set2))))
              ((> (car set1) (car set2))
               (intersection-set set1 (cdr set2))))))
  (define (union-set set1 set2)
    (cond ((null? set1) set2)
          ((null? set2) set1)
          ((< (car set1) (car set2))
           (cons (car set1) (union-set (cdr set1) set2)))
          ((= (car set1) (car set2))
           (union-set (cdr set1) set2))
          ((> (car set1) (car set2))
           (cons (car set2) (union-set set1 (cdr set2))))))

  ; interface to the rest of the system
  (put 'make-set 'ordered-list make-set)
  (put 'element-of-set? 'ordered-list element-of-set?)
  (put 'adjoin-set 'ordered-list adjoin-set)
  (put 'intersection-set '(ordered-list ordered-list) intersection-set)
  (put 'union-set '(ordered-list ordered-list) union-set)
  'done)

; install-binary-tree-pkg procedure
(define (install-binary-tree-pkg)
  ; helper internal procedures
  (define (intersection-set-impl set1 set2)
    (if (or (null? set1) (null? set2))
        '()
        (cond ((< (car set1) (car set2))
               (intersection-set-impl (cdr set1) set2))
              ((= (car set1) (car set2))
               (cons (car set1) (intersection-set-impl (cdr set1) (cdr set2))))
              ((> (car set1) (car set2))
               (intersection-set-impl set1 (cdr set2))))))
  (define (union-set-impl set1 set2)
    (cond ((null? set1) set2)
          ((null? set2) set1)
          ((< (car set1) (car set2))
           (cons (car set1) (union-set-impl (cdr set1) set2)))
          ((= (car set1) (car set2))
           (union-set-impl (cdr set1) set2))
          ((> (car set1) (car set2))
           (cons (car set2) (union-set-impl set1 (cdr set2))))))

  ; internal procedures
  (define (entry tree) (car tree))
  (define (left-branch tree) (cadr tree))
  (define (right-branch tree) (caddr tree))
  (define (make-tree entry left right)
    (list entry left right))
  (define (make-set elements) (list->tree elements))
  (define (element-of-set? x set)
    (cond ((null? set) false)
          ((= x (entry set)) true)
          ((< x (entry set))
           (element-of-set? x (left-branch set)))
          ((> x (entry set))
           (element-of-set? x (right-branch set)))))
  (define (adjoin-set x set)
    (cond ((null? set) (make-tree x '() '()))
          ((= x (entry set)) set)
          ((< x (entry set))
           (make-tree (entry set)
                      (adjoin-set x (left-branch set))
                      (right-branch set)))
          ((> x (entry set))
           (make-tree (entry set)
                      (left-branch set)
                      (adjoin-set x (right-branch set))))))
  (define (tree->list tree)
    (define (copy-to-list tree result-list)
      (if (null? tree)
          result-list
          (copy-to-list (left-branch tree)
                        (cons (entry tree)
                              (copy-to-list (right-branch tree)
                                            result-list)))))
    (copy-to-list tree '()))
  (define (list->tree elements)
    (car (partial-tree elements (length elements))))
  (define (partial-tree elts n)
    (if (= n 0)
        (cons '() elts)
        (let ((left-size (quotient (- n 1) 2)))
          (let ((left-result (partial-tree elts left-size)))
            (let ((left-tree (car left-result))
                  (non-left-elts (cdr left-result))
                  (right-size (- n (+ left-size 1))))
              (let ((this-entry (car non-left-elts))
                    (right-result (partial-tree (cdr non-left-elts)
                                                right-size)))
                (let ((right-tree (car right-result))
                      (remaining-elts (cdr right-result)))
                  (cons (make-tree this-entry left-tree right-tree)
                        remaining-elts))))))))
  (define (intersection-set set1 set2)
    (let ((A (tree->list set1)) (B (tree->list set2)))
      (list->tree (intersection-set-impl A B))))
  (define (union-set set1 set2)
    (let ((A (tree->list set1)) (B (tree->list set2)))
      (list->tree (union-set-impl A B))))

  ; interface to the rest of the system
  (put 'make-set 'binary-tree make-set)
  (put 'element-of-set? 'binary-tree element-of-set?)
  (put 'adjoin-set 'binary-tree adjoin-set)
  (put 'intersection-set '(binary-tree binary-tree) intersection-set)
  (put 'union-set '(binary-tree binary-tree) union-set)
  'done)

;
; Top-Level Abstraction Layer:
; ############################
;
; make-set constructor procedure
(define (make-set t elements)
  (attach-tag t ((get 'make-set t) elements)))

; element-of-set? predicate procedure
(define (element-of-set? x set)
  ((get 'element-of-set? (type-tag set)) x (contents set)))

; adjoin-set procedure
(define (adjoin-set x set)
  (attach-tag (type-tag set)
              ((get 'adjoin-set (type-tag set)) x (contents set))))

; intersection-set procedure
(define (intersection-set set1 set2)
  (attach-tag (type-tag set1)
              (apply-generic 'intersection-set set1 set2)))

; union-set procedure
(define (union-set set1 set2)
  (attach-tag (type-tag set1)
              (apply-generic 'union-set set1 set2)))

; find-employee-record procedure - Part (c)
(define (find-employee-record name files)
  (if (null? files)
      false
      (let ((file (car files)))
        (let ((tag (type-tag file)))
          (let ((result ((get 'get-record tag) name file)))
            (if result
                result
                (find-employee-record name (cdr files))))))))

;
; Part (d)
; ########
; Whenever Insatiable takes over another company, another installation package
; must be implemented. Since find-employee-record (lookup) is a generic
; procedure, no additional changes are required.

; unit tests
(install-unordered-list-pkg)
(install-ordered-list-pkg)
(install-binary-tree-pkg)
(define set1 (make-set 'binary-tree '(1 3 5 7 9 11)))
(define set2 (make-set 'binary-tree '(1 2 3 5 8 13)))
(define set3 (intersection-set set1 set2))
(define set4 (adjoin-set 21 set2))
(define set5 (union-set set1 set4))
(element-of-set? 7 set1)
(element-of-set? 7 set2)
set1
set2
set3
set4
set5
