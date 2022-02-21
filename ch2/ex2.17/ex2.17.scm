;
; last-pair: 'a list -> 'a list
; REQUIRES: xs is non-empty
; ENSURES: (last-pair xs) ==> last cons'ed pair in xs
(define (last-pair xs)
  (let ((tl (cdr xs)))
    (if (null? tl)
        xs
        (last-pair tl))))

(last-pair (list 23 79 149 34))
