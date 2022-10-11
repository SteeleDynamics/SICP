#|
 | Exercise 4.41
 |
 | Write an ordinary Scheme program to solve the multiple dwelling puzzle.
 |#

; enumerate-interval procedure
(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

; flatmap procedure
(define (flatmap proc seq)
  (fold-right append '() (map proc seq)))

; choices definition
(define choices
  (flatmap (lambda (a)
    (flatmap (lambda (b)
      (flatmap (lambda (c)
        (flatmap (lambda (d)
          (map (lambda (e) (list a b c d e))
               (enumerate-interval 1 5)))
          (enumerate-interval 1 5)))
        (enumerate-interval 1 5)))
      (enumerate-interval 1 5)))
    (enumerate-interval 1 5)))

; baker selector procedure
(define (baker choice) (car choice))

; cooper selector procedure
(define (cooper choice) (cadr choice))

; fletcher selector procedure
(define (fletcher choice) (caddr choice))

; miller selector procedure
(define (miller choice) (cadddr choice))

; smith selector procedure
(define (smith choice) (car (cddddr choice)))

; distinct? predicate procedure
(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

; pred predicate procedure
(define (pred choice)
  (and (distinct? choice)
       (not (= (baker choice) 5))
       (not (= (cooper choice) 1))
       (not (= (fletcher choice) 5))
       (not (= (fletcher choice) 1))
       (> (miller choice) (cooper choice))
       (not (= (abs (- (smith choice) (fletcher choice))) 1))
       (not (= (abs (- (fletcher choice) (cooper choice))) 1))))

; filter valid solution(s) using pred
(filter pred choices)
