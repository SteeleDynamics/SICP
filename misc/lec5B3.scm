;
; Structure and Interpretation of Computer Programs
; Computational Objects
; * * * * * * * * * *
; Lecture 5B
; Part 3 - Text Section 3.3
;
; At (53:07):
;
;  "Well so far, of course, it seems I have introduced several different
;   assignment operators: set!, set-car!, set-cdr!. Well maybe I should just
;   get rid of set-car! and set-cdr!. Maybe they're not worthwhile... Well
;   the answer is: 'Once you let the camel's nose into the tent, the rest of
;   him follows.' All I have to have is set!, and I can make all of the bad
;   things happen. Lets play with that a little bit..."
;
;   ~GJS
;
; Alonzo Church's Hack:
; #####################
; m -> message
; a -> the 'a' in car
; d -> the 'd' in cdr

(define (cons x y)
  (lambda (m) (m x y)))

(define (car x)
  (x (lambda (a d) a)))

(define (cdr x)
  (x (lambda (a d) d)))

;
; (car (cons 35 47))
; (car (lambda (m) (m 35 47)))
; ((lambda (m) (m 35 47)) (lambda (a d) a))
; ((lambda (a d) a) 35 47)
; 35

(car (cons 35 47))

;
; "Lambda Calculus" Mutable Data
; ##############################

(define (cons x y)
  (lambda (m)
    (m x
       y
       (lambda (n) (set! x n))
       (lambda (n) (set! y n)))))

(define (car x)
  (x (lambda (a d sa sd) a)))

(define (cdr x)
  (x (lambda (a d sa sd) d)))

(define (set-car! x y)
  (x (lambda (a d sa sd) (sa y))))

(define (set-cdr! x y)
  (x (lambda (a d sa sd) (sd y))))
