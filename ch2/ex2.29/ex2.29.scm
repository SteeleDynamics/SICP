; constructors
(define (make-mobile1 left right)
  (list left right))

(define (make-branch1 length structure)
  (list length structure))

; selectors
(define (left-branch1 m)
  (car m))

(define (right-branch1 m)
  (cadr m))

(define (branch-length1 b)
  (car b))

(define (branch-structure1 b)
  (cadr b))

; total-weight1 procedure
(define (total-weight1 m)
  (let ((left (branch-structure1 (left-branch1 m)))
        (right (branch-structure1 (right-branch1 m))))
    (cond ((and (not (pair? left)) (not (pair? right))) 
           (+ left right))
          ((and (not (pair? left)) (pair? right)) 
           (+ left (total-weight1 right)))
          ((and (pair? left) (not (pair? right))) 
           (+ (total-weight1 left) right))
          ((and (pair? left) (pair? right)) 
           (+ (total-weight1 left) (total-weight1 right))))))

; balanced1? procedure
(define (balanced1? m)
  (define (moment w d)
    (if (pair? w)
        (* (total-weight1 w) d)
        (* w d)))
  (let ((left (branch-structure1 (left-branch1 m)))
        (right (branch-structure1 (right-branch1 m)))
        (left-len (branch-length1 (left-branch1 m)))
        (right-len (branch-length1 (right-branch1 m))))
    (let ((top-balanced (= (moment left left-len)
                           (moment right right-len))))
      (cond ((and (not (pair? left)) (not (pair? right)))
             top-balanced)
            ((and (not (pair? left)) (pair? right))
             (and top-balanced (balanced1? right)))
            ((and (pair? left) (not (pair? right)))
             (and top-balanced (balanced1? left)))
            ((and (pair? left) (pair? right))
             (and top-balanced (balanced1? left) (balanced1? right)))))))

; constructors
(define (make-mobile2 left right)
  (cons left right))

(define (make-branch2 length structure)
  (cons length structure))

; selectors
(define (left-branch2 m)
  (car m))

(define (right-branch2 m)
  (cdr m))

(define (branch-length2 b)
  (car b))

(define (branch-structure2 b)
  (cdr b))

; total-weight2 procedure
(define (total-weight2 m)
  (let ((left (branch-structure2 (left-branch2 m)))
        (right (branch-structure2 (right-branch2 m))))
    (cond ((and (not (pair? left)) (not (pair? right))) 
           (+ left right))
          ((and (not (pair? left)) (pair? right)) 
           (+ left (total-weight2 right)))
          ((and (pair? left) (not (pair? right))) 
           (+ (total-weight2 left) right))
          ((and (pair? left) (pair? right)) 
           (+ (total-weight2 left) (total-weight2 right))))))

; balanced2? procedure
(define (balanced2? m)
  (define (moment w d)
    (if (pair? w)
        (* (total-weight2 w) d)
        (* w d)))
  (let ((left (branch-structure2 (left-branch2 m)))
        (right (branch-structure2 (right-branch2 m)))
        (left-len (branch-length2 (left-branch2 m)))
        (right-len (branch-length2 (right-branch2 m))))
    (let ((top-balanced (= (moment left left-len)
                           (moment right right-len))))
      (cond ((and (not (pair? left)) (not (pair? right)))
             top-balanced)
            ((and (not (pair? left)) (pair? right))
             (and top-balanced (balanced2? right)))
            ((and (pair? left) (not (pair? right)))
             (and top-balanced (balanced2? left)))
            ((and (pair? left) (pair? right))
             (and top-balanced (balanced2? left) (balanced2? right)))))))

; unit tests for balanced1? and balanced2?
(define b1 (make-branch1 1 2))
(define b2 (make-branch1 2 1))
(define m1 (make-mobile1 b1 b2))
(define b3 (make-branch1 2 3))
(define b4 (make-branch1 1 6))
(define m2 (make-mobile1 b3 b4))
(define b5 (make-branch1 6 m1))
(define b6 (make-branch1 2 m2))
(define m3 (make-mobile1 b5 b6))
(define b7 (make-branch1 3 m3))
(define b8 (make-branch1 8 4))
(define m4 (make-mobile1 b7 b8))

(total-weight1 m3)
(balanced1? m3)
(total-weight1 m4)
(balanced1? m4)

(define b9 (make-branch2 1 2))
(define b10 (make-branch2 2 1))
(define m5 (make-mobile2 b9 b10))
(define b11 (make-branch2 2 3))
(define b12 (make-branch2 1 6))
(define m6 (make-mobile2 b11 b12))
(define b13 (make-branch2 6 m5))
(define b14 (make-branch2 2 m6))
(define m7 (make-mobile2 b13 b14))
(define b15 (make-branch2 3 m7))
(define b16 (make-branch2 8 4))
(define m8 (make-mobile2 b15 b16))

(total-weight2 m7)
(balanced2? m7)
(total-weight2 m8)
(balanced2? m8)
