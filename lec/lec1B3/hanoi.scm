; Towers of Hanoi Algorithm
(define (move n from to spare)
  (cond ((= n 0) "done")
        (else
          (move (dec n) from spare to)
          (print-move from to)
          (move (dec n) spare to from))))

; (move 4 1 2 3)
; (move 3 1 3 2) ...
; (move 2 1 2 3) ...
; (move 1 1 3 2) ...
; ==> (tree-)recursive algorithm
;
; Has to take emponential time!
; Can you write an iterative algorithm?
