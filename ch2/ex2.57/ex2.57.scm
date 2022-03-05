; deriv procedure
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        ((exponentiation? exp)
         (if (= (exponent exp) 0)
             0
             (make-product
               (make-product (exponent exp)
                             (make-exponentiation (base exp)
                                                  (- (exponent exp) 1)))
               (deriv (base exp) var))))
        (else
         (error "unknown expression type -- DERIV" exp))))

; variable? predicate procedure
(define (variable? x) (symbol? x))

; same-variable? predicate procedure
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

; sum? predicate procedure
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

; make-sum constructor procedure
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

; addend selector procedure
(define (addend s) (cadr s))

; augend selector procedure --> modified!
(define (augend s)
  (if (null? (cdddr s))
      (caddr s)
      (make-sum (caddr s) (cadddr s))))

; product? predicate procedure
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

; =number? predicate procedure
(define (=number? exp num)
  (and (number? exp) (= exp num)))

; make-product constructor procedure
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

; multiplier selector procedure
(define (multiplier p) (cadr p))

; multiplicand selector procedure --> modified!
(define (multiplicand p)
  (if (null? (cdddr p))
      (caddr p)
      (make-product (caddr p) (cadddr p))))

; exponentiation? predicate procedure (power rule: integer exponent)
(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**) (number? (caddr x))))

; make-exponentiation constructor procedure
(define (make-exponentiation base exponent)
  (cond ((= exponent 0) 1)
        ((= exponent 1) base)
        ((number? base) (expt base exponent))
        (else (list '** base exponent))))

; base selector procedure
(define (base e) (cadr e))

; exponent selector procedure
(define (exponent e) (caddr e))

; unit tests
(deriv '(* x y (+ x 3)) 'x)
(deriv '(+ (* a (** x 3))
           (* b (** x 2))
           (* c (** x 1))
           (* d (** x 0)))
       'x)
