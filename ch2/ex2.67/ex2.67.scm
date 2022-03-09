; make-leaf constructor procedure
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

; leaf? predicate procedure
(define (leaf? object)
  (eq? (car object) 'leaf))

; symbol-leaf selector procedure
(define (symbol-leaf x) (cadr x))

; weight-leaf selector procedure
(define (weight-leaf x) (caddr x))

; make-code-tree constructor procedure
(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

; left-branch selector procedure
(define (left-branch tree) (car tree))

; right-branch selector procedure
(define (right-branch tree) (cadr tree))

; symbols selector procedure
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

; weight selector procedure
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

; decode procedure
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))


; choose-branch helper procedure
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

; adjoin-set procedure
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

; make-leaf-set procedure
(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

; given in problem statement
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

; given in problem statement
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

; output
(decode sample-message sample-tree)
