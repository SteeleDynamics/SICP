; nil defn
(define nil '())

; fold-left procedure
(define (fold-left op init seq)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter init seq))

; fold-right procedure
(define (fold-right op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (fold-right op init (cdr seq)))))

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

; term constructor and selector procedures
(define (make-term order coeff) (list order coeff))
(define (order term) (car term))
(define (coeff term) (cadr term))

; empty-termlist constructor and predicate procedures
(define (the-empty-termlist) '())
(define (empty-termlist? term-list) (null? term-list))

; install-sparse-pkg procedure
(define (install-sparse-pkg)
  ;; internal procedures
  (define (adjoin-sparse term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  (define (make-sparse terms)
    (if (null? terms)
        (the-empty-termlist)
        (adjoin-sparse (car terms) (make-sparse (cdr terms)))))
  (define (first-sparse term-list) (car term-list))
  (define (rest-sparse term-list) (cdr term-list))

  ;; interface to rest of the system
  (define (tag p) (attach-tag 'sparse p))
  (put 'adjoin-term '(term sparse)
       (lambda (term term-list) (tag (adjoin-sparse term term-list))))
  (put 'make-termlist 'sparse
       (lambda (terms) (tag (make-sparse terms))))
  (put 'first-term '(sparse) first-sparse)
  (put 'rest-terms '(sparse)
       (lambda (term-list) (tag (rest-sparse term-list))))
  'done)

; install-dense-pkg procedure
(define (install-dense-pkg)
  ;; internal procedures
  (define (adjoin-dense term term-list)
        (cons (coeff term) term-list))
  (define (make-dense terms)
    (if (null? terms)
        (the-empty-termlist)
        (adjoin-dense (car terms) (make-dense (cdr terms)))))
  (define (first-dense term-list)
    (make-term (- (length term-list) 1)
               (car term-list)))
  (define (rest-dense term-list) (cdr term-list))

  ;; interface to rest of the system
  (define (tag p) (attach-tag 'dense p))
  (put 'adjoin-term '(term dense)
       (lambda (term term-list) (tag (adjoin-dense term term-list))))
  (put 'make-termlist 'dense
       (lambda (terms) (tag (make-dense terms))))
  (put 'first-term '(dense) first-dense)
  (put 'rest-terms '(dense)
       (lambda (term-list) (tag (rest-dense term-list))))
  'done)

; generic adjoin-term procedure
(define (adjoin-term term term-list)
  (apply-generic 'adjoin-term (cons 'term term) term-list))

; generic first-term procedure
(define (first-term term-list)
  (apply-generic 'first-term term-list))

; generic rest-terms procedure
(define (rest-terms term-list)
  (apply-generic 'rest-terms term-list))

; make-sparse-termlist constructor procedure
(define (make-sparse-termlist . terms)
  ((get 'make-termlist 'sparse) terms))

; make-dense-termlist constructor procedure
(define (make-dense-termlist . terms)
  ((get 'make-termlist 'dense) terms))

; install-sparse-poly-pkg procedure
(define (install-sparse-poly-pkg)
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
  (define (zero-poly? p)
    (let ((L (term-list p)))
      (empty-termlist? L)))
  (define (neg-terms L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (cons (make-term (order (first-term L))
                         (neg (coeff (first-term L))))
              (neg-terms (rest-terms L)))))
  (define (neg-poly p)
    (let ((var (variable p))
          (lst (term-list p)))
      (make-poly var (neg-terms lst))))
  (define (sub-poly p1 p2)
    (add-poly p1 (neg-poly p2)))

  ;; interface to rest of the system
  (define (tag p) (attach-tag 'sparse-poly p))
  (put 'add '(sparse-poly sparse-poly)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(sparse-poly sparse-poly)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'sparse-poly
       (lambda (var terms) (tag (make-poly var terms))))
  (put '=zero? '(polynomial) zero-poly?)
  (put 'neg '(sparse-poly)
       (lambda (p) (tag (neg-poly p))))
  (put 'sub '(sparse-poly sparse-poly)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  'done)

; install-dense-poly-pkg procedure
(define (install-dense-poly-pkg)
  ;; internal procedures
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  (define (adjoin-term term term-list)
        (cons term term-list))
  (define (the-empty-termlist) '())
  (define (empty-termlist? term-list) (null? term-list))
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
            (cond ((> (length L1) (length L2))
                   (adjoin-term (car L1) (add-terms (cdr L1) L2)))
                 ((< (length L1) (length L2))
                  (adjoin-term (car L2) (add-terms L1 (cdr L2))))
                 (else
                  (adjoin-term
                    (add (car L1) (car L2))
                    (add-terms (cdr L1) (cdr L2))))))))
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
        (add-terms (append (mul-term-by-all-terms (car L1) L2)
                           (make-placeholders (length L1)))
                   (mul-terms (cdr L1) L2))))
  (define (mul-term-by-all-terms c1 L2)
    (if (empty-termlist? L2)
        (the-empty-termlist)
        (adjoin-term (mul c1 (car L2))
           (mul-term-by-all-terms c1 (cdr L2)))))
  (define (make-placeholders len)
    (define (iter i acc)
      (if (< i (- len 1))
          (iter (+ i 1) (cons 0 acc))
          acc))
    (iter 0 (the-empty-termlist)))
  (define (zero-poly? p)
    (let ((L (term-list p))
          (f (lambda (x acc) (and (=zero? x) acc))))
      (fold-right f true L)))
  (define (neg-terms L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (cons (neg (car L))
              (neg-terms (cdr L)))))
  (define (neg-poly p)
    (let ((var (variable p))
          (lst (term-list p)))
      (make-poly var (neg-terms lst))))
  (define (sub-poly p1 p2)
    (add-poly p1 (neg-poly p2)))

  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(dense-poly dense-poly)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(dense-poly dense-poly)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'dense-poly
       (lambda (var terms) (tag (make-poly var terms))))
  (put '=zero? '(polynomial) zero-poly?)
  (put 'neg '(dense-poly)
       (lambda (p) (tag (neg-poly p))))
  (put 'sub '(dense-poly dense-poly)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  'done)

; install-poly-pkg procedure
(define (install-poly-pkg)
  ;; internal procedures
  ; TODO: implement...

  ;; interface to rest of the system
  ; TODO: implement...
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
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))
(define (neg x) (apply-generic 'neg x))

; unit tests
(install-rectangular-package)
(install-polar-package)
(install-scheme-number-package)
(install-rational-package)
(install-complex-package)
(install-sparse-pkg)
(install-dense-pkg)

(define t16 (make-term 6 7))
(define t13 (make-term 3 1))
(define t12 (make-term 2 3))
(define t11 (make-term 1 3))
(define t10 (make-term 0 1))

(define t23 (make-term 3 4))
(define t22 (make-term 2 1))
(define t21 (make-term 1 2))
(define t20 (make-term 0 1))

(define s1 (make-sparse-termlist t13 t12 t11 t10))
(define s2 (make-sparse-termlist t22 t21 t20))
(define d1 (make-dense-termlist t13 t12 t11 t10))
(define d2 (make-dense-termlist t22 t21 t20))

s1
s2
d1
d2
(first-term s1)
(first-term s2)
(first-term d1)
(first-term d2)
(rest-terms s1)
(rest-terms s2)
(rest-terms d1)
(rest-terms d2)
(adjoin-term t16 s1)
(adjoin-term t23 d2)
