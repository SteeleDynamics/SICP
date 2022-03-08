;
; Appending '-impl' tag to sorted-list set representation procedures.

; element-of-set-impl? predicate procedure
(define (element-of-set-impl? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set-impl? x (cdr set)))))

; adjoin-set-impl procedure
(define (adjoin-set-impl x set)
  (cond ((null? set) (cons x set))
        ((< x (car set)) (cons x set))
        ((= x (car set)) set)
        (else (cons (car set) (adjoin-set-impl x (cdr set))))))

; intersection-set-impl procedure
(define (intersection-set-impl set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set-impl (cdr set1)
                                            (cdr set2))))
              ((< x1 x2)
               (intersection-set-impl (cdr set1) set2))
              ((< x2 x1)
               (intersection-set-impl set1 (cdr set2)))))))

; union-set-impl procedure
(define (union-set-impl set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((< (car set1) (car set2)) (cons (car set1)
                                         (union-set-impl (cdr set1) set2)))
        ((= (car set1) (car set2)) (union-set-impl (cdr set1) set2))
        ((> (car set1) (car set2)) (cons (car set2)
                                         (union-set-impl set1 (cdr set2))))))

; entry selector procedure
(define (entry tree) (car tree))

; left-branch selector procedure
(define (left-branch tree) (cadr tree))

; right-branch selector procedure
(define (right-branch tree) (caddr tree))

; make-tree constructor procedure
(define (make-tree entry left right)
  (list entry left right))

; element-of-set? predicate procedure
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

; adjoin-set procedure
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

; tree->list procedure
(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

; list->tree procedure
(define (list->tree elements)
  (car (partial-tree elements (length elements))))

; partial-tree procedure
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

; intersection-set procedure
(define (intersection-set set1 set2)
  (let ((A (tree->list set1)) (B (tree->list set2)))
    (list->tree (intersection-set-impl A B))))

; union-set procedure
(define (union-set set1 set2)
  (let ((A (tree->list set1)) (B (tree->list set2)))
    (list->tree (union-set-impl A B))))

; indent procedure
(define (indent d)
  (if (= d 0)
      (display "")
      ((lambda ()
         (display "  ")
         (indent (- d 1))))))

; print-tree procedure
(define (print-tree tree d)
  (newline)
  (indent d)
  (if (not (null? tree))
      ((lambda ()
         (display (entry tree))
         (print-tree (left-branch tree) (+ d 1))
         (print-tree (right-branch tree) (+ d 1))))
      (display "nil")))

; intersection-set and union-set unit tests
(define set1 (list->tree '(3 4 9 17 22 26 35 42)))
(define set2 (list->tree '(0 1 4 7 16 22 23 26 31)))
(define set3 (intersection-set set1 set2))
(define set4 (union-set set1 set2))
(print-tree set1 0)
(print-tree set2 0)
(print-tree set3 0)
(print-tree set4 0)
