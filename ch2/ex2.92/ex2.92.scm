; nil defn
(define nil '())

; append procedure
(define (append seq1 seq2)
  (if (null? seq1)
      seq2
      (cons (car seq1) (append (cdr seq1) seq2))))

; map procedure
(define (map proc seq)
  (if (null? seq)
      nil
      (cons (proc (car seq)) (map proc (cdr seq)))))

; filter procedure
(define (filter pred seq)
  (cond ((null? seq) nil)
        ((pred (car seq))
         (cons (car seq)
               (filter pred (cdr seq))))
        (else (filter pred (cdr seq)))))

; fold-right procedure (formerly known as accumulate)
(define (fold-right op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (fold-right op init (cdr seq)))))

; fold-left procedure
(define (fold-left op init seq)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter init seq))

; take procedure
(define (take seq n)
  (define (rec seq n)
    (if (= n 0)
        nil
        (cons (car seq)
              (rec (cdr seq) (- n 1)))))
  (if (or (< n 0)
          (> n (length seq)))
      (error "Bad subscript -- TAKE")
      (rec seq n)))

; drop procedure
(define (drop seq n)
  (define (rec seq n)
    (if (= n 0)
        seq
        (rec (cdr seq) (- n 1))))
  (if (or (< n 0)
          (> n (length seq)))
      (error "Bad subscript -- DROP")
      (rec seq n)))

; split procedure
(define (split seq)
  (cond ((null? seq) (list nil nil))
        ((null? (cdr seq)) (list seq nil))
        (else
          (let ((x (car seq))
                (y (cadr seq))
                (rem (cddr seq)))
            (let ((res (split rem)))
              (let ((A (car res))
                    (B (cadr res)))
                (list (cons x A) (cons y B))))))))

; merge procedure
(define (merge cmp seq1 seq2)
  (cond ((null? seq1) seq2)
        ((null? seq2) seq1)
        (else
          (let ((x (car seq1))
                (y (car seq2))
                (xs (cdr seq1))
                (ys (cdr seq2)))
            (if (cmp x y)
                (cons x (merge cmp xs seq2))
                (cons y (merge cmp seq1 ys)))))))

; msort procedure
(define (msort cmp seq)
  (cond ((null? seq) nil)
        ((null? (cdr seq)) seq)
        (else
          (let ((res (split seq)))
            (let ((A (car res))
                  (B (cadr res)))
              (merge cmp (msort cmp A) (msort cmp B)))))))

; compose procedure
(define (compose . procs)
  (define (rec procs arg)
    (if (null? procs)
        arg
        ((car procs) (rec (cdr procs) arg))))
  (lambda (arg) (rec procs arg)))

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

; instantiate operation-table, get/put procedures
(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

; attach-tag procedure
(define (attach-tag t contents)
  (cond ((pair? contents) (cons t contents))
        ((number? contents) contents)
        (else (error "Bad contents -- ATTACH-TAG" contents))))

; type-tag selector procedure
(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'scheme-number)
        (else (error "Bad tagged datum -- TYPE-TAG" datum))))

; contents selector procedure
(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        (else (error "Bad tagged datum -- CONTENTS" datum))))

; apply-generic procedure
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
            "No method for these types -- APPLY-GENERIC"
            (list op type-tags))))))

; install-rectangular-package procedure
(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

; install-polar-package procedure
(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

; generic selector procedures
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

; install-scheme-number package
(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'equ? '(scheme-number scheme-number) =)
  (put '=zero? '(scheme-number)
       (lambda (x) (= x 0)))
  (put 'neg '(scheme-number)
       (lambda (x) (tag (- x))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  'done)

; install-rational-package procedure
(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  (define (equ? x y)
    (and (= (numer x) (numer y))
         (= (denom x) (denom y))))
  (define (=zero? x)
    (= (numer x) 0))
  (define (neg-rat x)
    (make-rat (- (numer x)) (denom x)))

  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'equ? '(rational rational) equ?)
  (put '=zero? '(rational) =zero?)
  (put 'neg '(rational)
       (lambda (x) (tag (neg-rat x))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)

; install-complex-package procedure
(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))

  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  (define (equ? z1 z2)
    (and (= (real-part z1) (real-part z2))
         (= (imag-part z1) (imag-part z2))))
  (define (=zero? z)
    (and (= (real-part z) 0)
         (= (imag-part z) 0)))
  (define (neg-complex z)
    (make-from-real-imag (- (real-part z))
                         (- (imag-part z))))

  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'equ? '(complex complex) equ?)
  (put '=zero? '(complex) =zero?)
  (put 'neg '(complex)
       (lambda (z) (tag (neg-complex z))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))

  ; reference previous generic selectors for rectangular and polar pkgs
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  'done)

; install-polynomial-package procedure
(define (install-polynomial-package)
  ;; internal procedures
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- ADD-POLY"
               (list p1 p2))))
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1)) (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 (add-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2 (add-terms L1 (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term (order t1)
                                (add (coeff t1) (coeff t2)))
                     (add-terms (rest-terms L1)
                                (rest-terms L2)))))))))
  (define (sub-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (add-poly p1 (neg-poly p2)))
        (error "Polys not in same var -- SUB-POLY"
               (list p1 p2)))
  (define (neg-poly p)
    (let ((var (variable p))
          (lst (term-list p)))
      (make-poly var (neg-terms lst))))
  (define (neg-terms L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (cons (make-term (order (first-term L))
                         (neg (coeff (first-term L))))
              (neg-terms (rest-terms L)))))
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- MUL-POLY"
               (list p1 p2))))
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term (+ (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))
  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (let ((var (variable p1))
              (quot-rem (div-terms (term-list p1) (term-list p2))))
          (list (make-poly var (car quot-rem))
                (make-poly var (cadr quot-rem))))
        (error "Polys not in same var -- DIV-POLY"
               (list p1 p2))))
  (define (div-terms L1 L2)
    (if (empty-termlist? L1)
        (list (the-empty-termlist) (the-empty-termlist))
        (let ((t1 (first-term L1))
              (t2 (first-term L2)))
          (if (> (order t2) (order t1))
              (list (the-empty-termlist) L1)
              (let ((new-c (div (coeff t1) (coeff t2)))
                    (new-o (- (order t1) (order t2))))
                (let ((rest-of-result
                       (div-terms
                         (add-terms
                           L1
                           (neg-terms
                             (mul-term-by-all-terms
                               (make-term new-o new-c)
                               L2)))
                         L2)))
                  (list (cons (make-term new-o new-c)
                              (car rest-of-result))
                        (cadr rest-of-result))))))))
  (define (zero-poly? p)
    (let ((L (term-list p)))
      (empty-termlist? L)))
  (define (expand-poly p)
    (let ((var (variable p))
          (lst (term-list p)))
      (let ((fcn (lambda (term acc) (append (expand-term var term) acc))))
        (fold-right fcn nil lst))))
  (define (expand-term var term)
    (let ((ord (order term))
          (cff (coeff term)))
      (let ((make-poly-var (lambda (trm) (tag (make-poly var (list trm))))))
        (if (and (pair? cff)
                 (eq? (car cff) 'polynomial))
            (map make-poly-var (expand-coeff ord cff))
            (list (make-poly-var term))))))
  (define (expand-coeff ord cff)
    (let ((make-term-ord (lambda (cof) (make-term ord cof))))
      (map make-term-ord (expand-poly (contents cff)))))
  (define (factors<-poly p)
    (let ((var (variable p))
          (trm (first-term (term-list p))))
      (let ((ord (order trm))
            (cff (coeff trm)))
        (let ((factor (list var ord)))
          (if (and (pair? cff)
                   (eq? (car cff) 'polynomial))
              (cons factor (factors<-poly (contents cff)))
              (cons factor (list cff)))))))
  (define (var-factor-cmp f1 f2)
    (let ((var1 (car f1))
          (var2 (car f2)))
      (symbol<? var1 var2)))
  (define (sort-factors fs)
    (let ((var-factors (take fs (- (length fs) 1)))
          (conc-factor (drop fs (- (length fs) 1))))
      (let ((ord-factors (msort var-factor-cmp var-factors)))
        (append ord-factors conc-factor))))
  (define (poly<-factors fs)
    (define (fcn x acc)
      (tag (make-poly (car x) (list (make-term (cadr x) acc)))))
    (let ((var-factors (take fs (- (length fs) 1)))
          (conc-factor (car (drop fs (- (length fs) 1)))))
      (fold-right fcn conc-factor var-factors)))
  (define (reorder-poly p)
    (let ((polys (expand-poly p))
          (reord (compose poly<-factors sort-factors factors<-poly contents)))
      (let ((ord-polys (map reord polys))
            (fcn (lambda (acc x) (add acc x))))
        (fold-left fcn (car ord-polys) (cdr ord-polys)))))

  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'neg '(polynomial)
       (lambda (p) (tag (neg-poly p))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'div '(polynomial polynomial)
       (lambda (p1 p2) (map tag (div-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  (put '=zero? '(polynomial) zero-poly?)
  (put 'reorder '(polynomial)
       (lambda (p) (reorder-poly p)))
  'done)

; make-scheme-number constructor procedure
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

; make-rational constructor procedure
(define (make-rational n d)
  ((get 'make 'rational) n d))

; make-complex-from-real-imag constructor procedure
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

; make-complex-from-mag-ang constructor procedure
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

; make-polynomial constructor procedure
(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

; generic operator procedures
(define (add x y) (apply-generic 'add x y))
(define (neg x) (apply-generic 'neg x))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))
(define (reorder x) (apply-generic 'reorder x))

; unit tests
(install-rectangular-package)
(install-polar-package)
(install-scheme-number-package)
(install-rational-package)
(install-complex-package)
(install-polynomial-package)

(define p0 (make-polynomial 'x '()))
(define p1 (make-polynomial 'x '((3 1) (2 3) (1 3) (0 1))))
(define p2 (make-polynomial 'x '((2 1) (1 2) (0 1))))
(define p3 (make-polynomial 'x '((0 1))))
(define p4 (make-polynomial 'y (list (list 2 p3) (list 1 p2) (list 0 p1))))
(define p5 (reorder p4))
(define p6 (reorder p5))
(define p7 (reorder p1))
(define p8 (reorder p2))
(=zero? p0)
(div p1 p2)
p4
p5
p6
p7
p8

;
; At this point, we could create a locally-scoped procedure in add-poly and
; mul-poly that reorders each p1 and p2 and then add/mul the contents of those 
; reordered polys together. However, this is trivial in comparison to
; implementing merge-sort and polynomial expansion/reordering. Stopping at this
; point is sufficient. Procedures created:
;
; 1.  (take seq n) - return the first n elts of seq
; 2.  (drop seq n) - returns what is left after dropping the first n elts of seq
; 3.  (split seq) - split seq into two lists, lengths differ by at most 1
; 4.  (merge cmp seq1 seq2) - merge seq1 and seq2 together in cmp-order 
; 5.  (msort cmp seq) - merge sort elements in cmp-order
; 6.  (compose f1 f2 ...) - function composition of f1, f2, ...
; 7.  (expand-poly p) - recursively expand all terms
; 8.  (expand-term var term) - recursively expand coefficient or return term
; 9.  (expand-coeff ord cff) - recursively expand coefficient (expand-poly)
; 10. (var-factor-cmp f1 f2) - predicate that compares factors f1 and f2
; 11. (factors<-poly p) - given a fully expanded poly p, separate into factors
; 12. (sort-factors fs) - sort a list of factors (except concrete factor, end)
; 13. (poly<-factors fs) - combine factors into single polynomial
; 14. (reord-poly p) - expand p, reorder expanded polys, and add reordered polys
;
; Note: (reorder p) ==> p iff p is already sorted or only has one variable. See
;       the unit tests above for examples.
;
