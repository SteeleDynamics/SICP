; fact-cps procedure
(define fact-cps
  (lambda (n)
    (lambda (sc)
      (lambda (fc)
        (cond ((> n 0)
               (((fact-cps (- n 1)) (lambda (res) (sc (* n res)))) fc))
              ((= n 0)
               (sc 1))
              (else
                (fc)))))))

; exp-cps procedure
(define exp-cps
  (lambda (n)
    (lambda (sc)
      (lambda (fc)
        (cond ((> n 0)
               (((exp-cps (- n 1)) (lambda (res) (sc (* 2 res)))) fc))
              ((= n 0)
               (sc 1))
              (else
                (fc)))))))

; success and failure continuation procedures
(define (sc x) (list 'SOME x))
(define (fc) (list 'NONE))

; fact-cps unit tests
(((fact-cps 6) sc) fc)
(((fact-cps 0) sc) fc)
(((fact-cps -1) sc) fc)

; exp-cps unit tests
(((exp-cps 16) sc) fc)
(((exp-cps 8) sc) fc)
(((exp-cps -1) sc) fc)

#|
 | Let n = 3 and sc = (lambda (x) x):
 |
 |     (((fact-cps 3) (lambda (x) x)) fc)
 | ==> (((fact-cps 2) (lambda (res) ((lambda (x) x) (* 3 res)))) fc)
 | ==> (((fact-cps 1) (lambda (res) ((lambda (res) ((lambda (x) x) (* 3 res))) (* 2 res)))) fc)
 | ==> (((fact-cps 0) (lambda (res) ((lambda (res) ((lambda (res) ((lambda (x) x) (* 3 res))) (* 2 res))) (* 1 res)))) fc)
 | ==> ((lambda (res) ((lambda (res) ((lambda (res) ((lambda (x) x) (* 3 res))) (* 2 res))) (* 1 res))) 1)
 | ==> ((lambda (res) ((lambda (res) ((lambda (x) x) (* 3 res))) (* 2 res))) (* 1 1))
 | ==> ((lambda (res) ((lambda (x) x) (* 3 res))) (* 2 1))
 | ==> ((lambda (x) x) (* 3 2))
 | ==> 6
 |#
