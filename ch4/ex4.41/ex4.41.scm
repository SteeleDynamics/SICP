#|
 | Exercise 4.41
 |
 | Write an ordinary Scheme program to solve the multiple dwelling puzzle.
 |#

; flatmap procedure
(define (flatmap proc seq)
  (fold-right append '() (map proc seq)))

; permutations procedure
(define (permutations seqs)
  (if (null? seqs)
      '(())
      (flatmap
        (lambda (x)
          (map
            (lambda (y) (cons x y))
            (permutations (cdr seqs))))
        (car seqs))))

; choices definition
(define choices
  (permutations '((1 2 3 4 5)
                  (1 2 3 4 5)
                  (1 2 3 4 5)
                  (1 2 3 4 5)
                  (1 2 3 4 5))))

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
