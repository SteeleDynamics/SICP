; nil defn
(define nil '())

; memq procedure
(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

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
        nil
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
      nil
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

; encode procedure
(define (encode message tree)
  (if (null? message)
      nil
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

; encode-symbol procedure
(define (encode-symbol x tree)
  (cond ((leaf? tree) nil)
        ((pair? (memq x (symbols (left-branch tree))))
         (cons 0 (encode-symbol x (left-branch tree))))
        (else (cons 1 (encode-symbol x (right-branch tree))))))

; generate-huffman-tree procedure
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

; successive-merge procedure
(define (successive-merge set)
  (if (null? (cdr set))
      (car set)
      (successive-merge
        (adjoin-set
          (make-code-tree (car set) (cadr set))
          (cddr set)))))

; encoding message using Huffman algorithm
(define pairs '((A 2) (BOOM 1) (GET 2) (JOB 2) (NA 16) (SHA 3) (YIP 9) (WAH 1)))
(define tree (generate-huffman-tree pairs))
(define message '(Get a job
                  Sha na na na na na na na na
                  Get a job
                  Sha na na na na na na na na
                  Wah yip yip yip yip yip yip yip yip yip
                  Sha boom))
(length (encode message tree))

; fixed-length encoding: 3 bits/symbol * 36 symbols/msg = 108 bits/msg
