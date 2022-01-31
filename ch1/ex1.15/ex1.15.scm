(define (cube x) (* x x x))

(define (p x) (- (* 3 x) (* 4 (cube x))))

(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

;
; (sine 12.15)
; (p (sine 4.05))
; (p (p (sine 1.35)))
; (p (p (p (sine 0.45))))
; (p (p (p (p (sine 0.15)))))
; (p (p (p (p (p (sine 0.05))))))
; (p (p (p (p (p 0.05)))))
; (p (p (p (p 0.1495))))
; (p (p (p 0.435134551)))
; (p (p 0.975846534))
; (p -0.789563119)
; -0.399803437
;
; a) p is applied 5 times when computing (sine 12.15)
; b) Time complexity --> T(a) = O(log3(a))
;    Space Complexity --> S(a) = O(log3(a))

(sine 12.15)
