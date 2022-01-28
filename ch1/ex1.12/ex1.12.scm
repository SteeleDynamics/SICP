;
; Pascal's Triangle
;
;   k 0 1 2 3 4 ..
; n
; 0   1
; 1   1 1
; 2   1 2 1
; 3   1 3 3 1
; 4   1 4 6 4 1
; :   ...

(define (pascal n k)
  (cond ((= k 0) 1)
        ((= k n) 1)
        (else (+ (pascal (- n 1) (- k 1))
                 (pascal (- n 1) k)))))

(pascal 4 2)

