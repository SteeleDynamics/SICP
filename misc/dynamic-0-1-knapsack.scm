;; load run-time option 'format
(load-option 'format)

;; last? predicate
(define (last? lis) (null? (cdr lis)))

;; last selector
(define (last lis)
  (cond ((null? lis) (error #f "Empty list -- LAST"))
        ((last? lis) (car lis))
        (else (last (cdr lis)))))

;; next-to-last? predicate
(define (next-to-last? lis) (null? (cddr lis)))

;; next-to-last selector
(define (next-to-last lis)
  (cond ((null? lis) (error #f "Empty list -- NEXT-TO-LAST"))
        ((next-to-last? lis) (car lis))
        (else (next-to-last (cdr lis)))))

;; make-strides procedure
(define (make-strides dims)
  (if (last? dims)
      '(1)
      (cons (fold-left * 1 (cdr dims))
            (make-strides (cdr dims)))))

;; make-array constructor
(define (make-array dims)
  (list dims
        (make-strides dims)
        (make-vector (fold-left * 1 dims))))

;; vector->array procedure
(define (vector->array vec dims)
  (list dims
        (make-strides dims)
        (vector-copy vec)))

;; array-dims selector
(define (array-dims arr) (car arr))

;; array-strides selector
(define (array-strides arr) (cadr arr))

;; array-vector selector
(define (array-vector arr) (caddr arr))

;; inner-prod procedure
(define (inner-prod x y acc)
  (if (or (null? x) (null? y))
      acc
      (inner-prod (cdr x) (cdr y) (+ (* (car x) (car y)) acc))))

;; array-ref procedure
(define (array-ref arr ind)
  (vector-ref (array-vector arr)
              (inner-prod (array-strides arr) ind 0)))

;; array-set! procedure
(define (array-set! arr ind val)
  (vector-set! (array-vector arr)
               (inner-prod (array-strides arr) ind 0)
               val))

;; subvector-disp procedure
(define (subvector-disp vec beg end w)
  (do ((i beg (+ i 1)))
      ((>= i end))
    (display (format #f "~v@a" w (vector-ref vec i))))
  (newline))

;; vector-disp-iter procedure
(define (vector-disp-iter vec len pos m n w)
  (cond ((zero? m)
         (subvector-disp vec pos n w))
        ((zero? pos)
         (subvector-disp vec pos (+ pos n) w)
         (vector-disp-iter vec len (+ pos n) m n w))
        (else
         (cond ((= pos (- len n))
                (subvector-disp vec pos (+ pos n) w))
               ((zero? (remainder (+ pos n) (* m n)))
                (subvector-disp vec pos (+ pos n) w)
                (newline)
                (vector-disp-iter vec len (+ pos n) m n w))
               (else
                (subvector-disp vec pos (+ pos n) w)
                (vector-disp-iter vec len (+ pos n) m n w))))))

;; array-disp procedure
(define (array-disp arr w)
  (let* ((vec (array-vector arr))
         (dims (array-dims arr))
         (len (vector-length vec))
         (rank (length dims))
         (m (if (> rank 1) (next-to-last dims) 0))
         (n (last dims)))
    (newline)
    (vector-disp-iter vec len 0 m n w)))

;; dynamic-0-1-knapsack procedure
(define (dynamic-0-1-knapsack v-arr w-arr n W)
  (let ((c (make-array (list (+ n 1) (+ W 1)))))
    (do ((j 0 (+ j 1)))
        ((> j W))
      (array-set! c (list 0 j) 0))
    (do ((i 1 (+ i 1)))
        ((> i n) c)
      (array-set! c (list i 0) 0)
      (do ((j 1 (+ j 1)))
          ((> j W) c)
        (let ((wi (array-ref w-arr (list (- i 1))))
              (cu (array-ref c (list (- i 1) j))))             ; c[i,j] up elt
          (if (<= wi j)
              (let ((vi (array-ref v-arr (list (- i 1))))
                    (cd (array-ref c (list (- i 1) (- j wi))))); c[i,j] diag elt
                (if (> (+ vi cd) cu)
                    (array-set! c (list i j) (+ vi cd))
                    (array-set! c (list i j) cu)))
              (array-set! c (list i j) cu)))))))

;; trace-0-1-knapsack
(define (trace-0-1-knapsack c-arr w-arr ind acc)
  (if (zero? (car ind))
      acc
      (let* ((i (car ind))
             (w (cadr ind))
             (up-ind (list (- i 1) w))
             (cc (array-ref c-arr ind))
             (cu (array-ref c-arr up-ind)))
        (if (= cc cu)
            (trace-0-1-knapsack c-arr w-arr up-ind acc)
            (let* ((wi (array-ref w-arr (list (- i 1))))
                   (diag-ind (list (- i 1) (- w wi))))
              (trace-0-1-knapsack c-arr w-arr diag-ind (cons i acc)))))))

;; unit tests
(define v-arr (vector->array '#(6 10 12) '(3)))
(define w-arr (vector->array '#(1 2 3) '(3)))
(define n 3)
(define W 5)
(define c-arr (dynamic-0-1-knapsack v-arr w-arr n W))
(define res (trace-0-1-knapsack c-arr w-arr (list n W) '()))
(define arr1 (vector->array '#(0 1 2 3 4 5 6 7 8 9 a b c d e f g h
                                 i j k l m n o p q r s t u v w x y z)
                            '(3 2 3 2)))
(define arr2 (vector->array '#(0 1 2 3 4 5 6 7 8 9 a b c d e f g h
                                 i j k l m n o p q r s t u v w x y z)
                            '(2 2 3 3)))
(array-disp v-arr 3)
(array-disp v-arr 3)
n
W
(array-disp c-arr 3)
res
(array-dims arr1)
(array-disp arr1 3)
(array-dims arr2)
(array-disp arr2 3)
