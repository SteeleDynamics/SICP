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

;
; Part (a)
; ########
; 1. At each level of recursion, partial-tree returns a balanced binary tree
;    for the n items in elts.
; 2. partial-tree builds a balanced subtree from right to left.
; 3. Subsequent subtrees are then recursively built
;
;                                        5
;                                      /   \
; (list->tree '(1 3 5 7 9 11)) --->   1     9
;                                      \   / \
;                                       3 7  11

(define (indent d)
  (if (= d 0)
      (display "")
      ((lambda ()
         (display "  ")
         (indent (- d 1))))))

(define (print-tree tree d)
  (newline)
  (indent d)
  (if (not (null? tree))
      ((lambda ()
         (display (entry tree))
         (print-tree (left-branch tree) (+ d 1))
         (print-tree (right-branch tree) (+ d 1))))
      (display "nil")))

(print-tree (list->tree '(1 3 5 7 9 11)) 0)

;
; Part (b)
; ########
; W_i = 2*W(n/(2^i)) + k1
;
; log(n)
; Σ(W_i) = k1*(2^i) ==> O(n)  (geometric series)
; i = 0
