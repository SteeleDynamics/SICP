;
; implementation of for-each

(define (for-each proc items)
  (cond ((null? items) true)
        (else (proc (car items)) (for-each proc (cdr items)))))

; unit test of for-each
(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))
