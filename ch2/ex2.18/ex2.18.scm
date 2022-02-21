; reverse : 'a list -> 'a list
; REQUIRES: true
; ENSURES: (reverse xs) ==>* list containing exactly the elements of xs, in the
;          opposite order they appeared in xs
(define (reverse xs)
  (define (trev xs acc)
    (if (null? xs)
        acc
        (trev (cdr xs) (cons (car xs) acc))))
  (trev xs (list)))

; unit test of reverse/trev
(reverse (list 1 4 9 16 25))
