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


(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

;
; Part (a)
; ########
;   Both tree-list-1 and tree-list-2 produce an in-order tree walk. Below is the
;   output for the trees in figure 2.16:

(define tree1
  (make-tree 7
    (make-tree 3
      (make-tree 1 '() '())
      (make-tree 5 '() '()))
    (make-tree 9
      '()
      (make-tree 11 '() '()))))

(define tree2
  (make-tree 3
    (make-tree 1 '() '())
    (make-tree 7
      (make-tree 5 '() '())
      (make-tree 9
        '()
        (make-tree 11 '() '())))))

(define tree3
  (make-tree 5
    (make-tree 3
      (make-tree 1 '() '())
      '())
    (make-tree 9
      (make-tree 7 '() '())
      (make-tree 11 '() '()))))

(tree->list-1 tree1)
(tree->list-2 tree1)
(tree->list-1 tree2)
(tree->list-2 tree2)
(tree->list-1 tree3)
(tree->list-2 tree3)

;
; Part (b)
; ########
;
;   tree-list-1:
;   ------------
;                                       C2
;                   ┌───────────────────┴───────────────────┐
;              C1*(n/2) + C2                           C1*(n/2) + C2
;         ┌─────────┴─────────┐                   ┌─────────┴─────────┐
;    C1*(n/4) + C2       C1*(n/4) + C2       C1*(n/4) + C2       C1*(n/4) + C2
;    ┌────┴────┐         ┌────┴────┐         ┌────┴────┐         ┌────┴────┐
; C1*(n/8)  C1*(n/8)  C1*(n/8)  C1*(n/8)  C1*(n/8)  C1*(n/8)  C1*(n/8)  C1*(n/8)
;   + C2      + C2      + C2      + C2      + C2      + C2      + C2      + C2
; ┌──┴──┐   ┌──┴──┐   ┌──┴──┐   ┌──┴──┐   ┌──┴──┐   ┌──┴──┐   ┌──┴──┐   ┌──┴──┐
;                                     ....
;   log(n)
;   Σ(W_i) = C1*n + C2 ==> O(n*log(n))
;   i = 0
;
;   tree-list-2:
;   ------------
;                                       C1
;                   ┌───────────────────┴───────────────────┐
;                   C1                                      C1
;         ┌─────────┴─────────┐                   ┌─────────┴─────────┐
;         C1                  C1                  C1                  C1
;    ┌────┴────┐         ┌────┴────┐         ┌────┴────┐         ┌────┴────┐
;    C1        C1        C1        C1        C1        C1        C1        C1
; ┌──┴──┐   ┌──┴──┐   ┌──┴──┐   ┌──┴──┐   ┌──┴──┐   ┌──┴──┐   ┌──┴──┐   ┌──┴──┐
;                                     ....
;   log(n)
;   Σ(W_i) = C1 * (2^i) ==> O(n)
;   i = 0
