; Peano Arithmetic Examples

(define (+ a b)
  (if (= a 0)
      b
      (inc (+ (dec a) b))))

; (+ 5 3)
; (inc (+ 4 3))
; (inc (inc (+ 3 3)))
; (inc (inc (inc (+ 2 3))))
; (inc (inc (inc (inc (+ 1 3)))))
; (inc (inc (inc (inc (inc (+ 0 3))))))
; (inc (inc (inc (inc (inc 3)))))
; (inc (inc (inc (inc 4))))
; (inc (inc (inc 5)))
; (inc (inc 6))
; (inc 7)
; 8
; recursive process

(define (+ a b)
  (if (= a 0)
      b
      (+ (dec a) (inc b))))

; (+ 5 3)
; (+ 4 4)
; (+ 3 5)
; (+ 2 6)
; (+ 1 7)
; (+ 0 8)
; 8
; iterative process
