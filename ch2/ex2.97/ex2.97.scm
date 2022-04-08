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
  ;; internal procedures
  (define (reduce-integers n d)
    (let ((g (gcd n d)))
      (list (/ n g) (/ d g))))

  ;; interface to rest of the system
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
  (put 'greatest-common-divisor '(scheme-number scheme-number)
       (lambda (x y) (tag (gcd x y))))
  (put '=zero? '(scheme-number)
       (lambda (x) (= x 0)))
  (put 'neg '(scheme-number)
       (lambda (x) (tag (- x))))
  (put 'reduce '(scheme-number scheme-number)
       (lambda (x y) (map tag (reduce-integers x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  'done)

; install-rational-package procedure
(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cadr x))
  (define (make-rat n d)
    (reduce n d))
  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
              (mul (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (mul (numer x) (denom y))
              (mul (denom x) (numer y))))
  (define (=zero-rat? x)
    (=zero? (numer x)))
  (define (neg-rat x)
    (make-rat (neg (numer x)) (denom x)))

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
  (put '=zero? '(rational) =zero-rat?)
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
  (define (gcd-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (let ((var (variable p1))
              (a (term-list p1))
              (b (term-list p2)))
          (make-poly var (gcd-terms a b)))
        (error "Polys not in same var -- GCD-POLY"
               (list p1 p2))))
  (define (gcd-terms a b)
    (if (empty-termlist? b)
        (normalize-coeff a)
        (gcd-terms b (pseudoremainder-terms a b))))
  (define (normalize-coeff a)
    (let ((g (apply gcd (map coeff a))))
      (let ((t (make-term 0 (/ 1 g))))
        (mul-term-by-all-terms t a))))
  (define (pseudoremainder-terms a b)
    (let ((O1 (order (first-term a)))
          (O2 (order (first-term b)))
          (c (coeff (first-term b))))
      (let ((integerizing-factor (expt c (+ 1 O1 (- O2)))))
        (let ((t (make-term 0 integerizing-factor)))
          (let ((pseudo-a (mul-term-by-all-terms t a)))
            (cadr (div-terms pseudo-a b)))))))
  (define (reduce-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (let ((var (variable p1))
              (res (reduce-terms (term-list p1) (term-list p2))))
          (list (make-poly var (car res))
                (make-poly var (cadr res))))
        (error "Polys not in same var -- REDUCE-POLY"
               (list p1 p2))))
  (define (reduce-terms n d)
    (define (make-integerizing-factor n d g)
      (let ((O1n (order (first-term n)))
            (O1d (order (first-term d))))
        (let ((O1 (max O1n O1d))
              (O2 (order (first-term g)))
              (c (coeff (first-term g))))
          (expt c (+ 1 O1 (- O2))))))
    (define (pseudo-reduce n d g integerizing-factor)
      (let ((t1 (make-term 0 integerizing-factor)))
        (let ((n1 (mul-term-by-all-terms t1 n))
              (d1 (mul-term-by-all-terms t1 d)))
          (let ((n2 (car (div-terms n1 g)))
                (d2 (car (div-terms d1 g))))
            (list n2 d2)))))
    (define (finalize-reduce n d)
      (let ((cs (append (map coeff n) (map coeff d))))
        (let ((g (apply gcd cs)))
          (let ((t (make-term 0 (/ 1 g))))
            (let ((nn (mul-term-by-all-terms t n))
                  (dd (mul-term-by-all-terms t d)))
              (list nn dd))))))
    (let ((g (gcd-terms n d)))
      (let ((integerizing-factor (make-integerizing-factor n d g)))
        (let ((pseudo-res (pseudo-reduce n d g integerizing-factor)))
          (let ((pseudo-n (car pseudo-res))
                (pseudo-d (cadr pseudo-res)))
            (finalize-reduce pseudo-n pseudo-d))))))
  (define (zero-poly? p)
    (let ((L (term-list p)))
      (empty-termlist? L)))

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
  (put 'greatest-common-divisor '(polynomial polynomial)
       (lambda (p1 p2) (tag (gcd-poly p1 p2))))
  (put 'reduce '(polynomial polynomial)
       (lambda (p1 p2) (map tag (reduce-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  (put '=zero? '(polynomial) zero-poly?)
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
(define (greatest-common-divisor x y)
  (apply-generic 'greatest-common-divisor x y))
(define (neg x) (apply-generic 'neg x))
(define (reduce x y) (apply-generic 'reduce x y))
(define (=zero? x) (apply-generic '=zero? x))

; unit tests
(install-rectangular-package)
(install-polar-package)
(install-scheme-number-package)
(install-rational-package)
(install-complex-package)
(install-polynomial-package)

(define p1 (make-polynomial 'x '((1 1) (0 1))))
(define p2 (make-polynomial 'x '((3 1) (0 -1))))
(define p3 (make-polynomial 'x '((1 1))))
(define p4 (make-polynomial 'x '((2 1) (0 -1))))

(define rf1 (make-rational p1 p2))
(define rf2 (make-rational p3 p4))

(add rf1 rf2)
