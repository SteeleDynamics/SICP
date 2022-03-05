; car-list procedure
(define (car-list seq)
  (if (null? (cdr seq))
      (car seq)
      seq))

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
        (else
         (error "unknown expression type -- DERIV" exp))))

; variable? predicate procedure
(define (variable? x) (symbol? x))

; same-variable? predicate procedure
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

; sum? predicate procedure
(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))

; make-sum constructor procedure
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

; addend selector procedure
(define (addend s) (car s))

; augend selector procedure
(define (augend s) (car-list (cddr s)))

; product? predicate procedure
(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

; =number? predicate procedure
(define (=number? exp num)
  (and (number? exp) (= exp num)))

; make-product constructor procedure
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

; multiplier selector procedure
(define (multiplier p) (car p))

; multiplicand selector procedure
(define (multiplicand p) (car-list (cddr p)))

; unit tests
(deriv '(x + 3 * (x + y + 2)) 'x)
(deriv '(x * y * (x + 3)) 'x)
