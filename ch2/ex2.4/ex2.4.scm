(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))

;
; (car (cons 'fst 'snd))
; (car (lambda (m) (m 'fst 'snd)))
; ((lambda (m) (m 'fst 'snd)) (lambda (p q) p))
; ((lambda (p q) p) 'fst 'snd)
; 'fst
;
; (cdr (cons 'fst 'snd))
; (cdr (lambda (m) (m 'fst 'snd)))
; ((lambda (m) (m 'fst 'snd)) (lambda (p q) q))
; ((lambda (p q) p) 'fst 'snd)
; 'snd


(car (cons 'fst 'snd))
(cdr (cons 'fst 'snd))
