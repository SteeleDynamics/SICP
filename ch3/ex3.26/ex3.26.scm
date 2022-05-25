;
; Exercise 3.26
; #############
;
; To search a table as implemented above, one needs to scan through the list of
; records. This is basically the unordered list representation of section 2.3.3.
; For large tables, it may be more efficient to structure the table in a
; different manner. Describe a table implementation where the (key, value)
; records are organized using a binary tree, assuming that keys can be ordered
; in some way (e.g., numerically or alphabetically). (Compare exercise 2.66 of
; chapter 2.)
;
; --> Code from exercise 2.66
;

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

; record constructor procedure
(define (make-record key field1 field2 field3)
  (list key field1 field2 field3))

; key selector procedure
(define (key record)
  (car record))

; field1 selector procedure
(define (field1 record)
  (cadr record))

; field2 selector procedure
(define (field2 record)
  (caddr record))

; field3 selector procedure
(define (field3 record)
  (cadddr record))

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((= given-key (key (entry set-of-records)))
         (entry set-of-records))
        ((< given-key (key (entry set-of-records)))
         (lookup given-key (left-branch set-of-records)))
        (else
         (lookup given-key (right-branch set-of-records)))))

; unit tests
(define r0 (make-record 0 "Alyssa" "P." "Hacker"))
(define r1 (make-record 1 "Ben" "" "Bitdiddle"))
(define r2 (make-record 2 "Cy" "D." "Fect"))
(define r3 (make-record 3 "Eva" "Lu" "Ator"))
(define r4 (make-record 4 "Lem" "E." "Tweakit"))
(define r5 (make-record 5 "Louis" "" "Reasoner"))
(define set-of-records (list->tree (list r0 r1 r2 r3 r4 r5)))
(print-tree set-of-records 0)
(lookup -1 set-of-records)
(lookup 0 set-of-records)
(lookup 1 set-of-records)
(lookup 2 set-of-records)
(lookup 3 set-of-records)
(lookup 4 set-of-records)
(lookup 5 set-of-records)
(lookup 6 set-of-records)
