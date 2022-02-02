;
; AXIOM FOR PAIRS
;
; For any x and y
; 
; (car (cons x y)) is x
; (cdr (cons x y)) is y
;

(define (cons a b)
  (lambda (pick)
    (cond ((= pick 1) a)
          ((= pick 2) b))))

(define (car x) (x 1))

(define (cdr x) (x 2))

;
; (car (cons 37 49))
; (car (lambda (pick)
;        (cond ((= pick 1) 37)
;              ((= pick 2) 49))))
; ((lambda (pick)
;    (cond ((= pick 1) 37)
;          ((= pick 2) 49)))
; 1)
; (cond ((= 1 1) 37)
;       ((= 1 2) 49))
; 37

(car (cons 37 49))
