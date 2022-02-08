(define (expmod1 base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod1 base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod1 base (- exp 1) m))
                    m))))        

(define (expmod2 base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (* (expmod2 base (/ exp 2) m)
                       (expmod2 base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod2 base (- exp 1) m))
                    m))))

;
; Applicative-Order Evaluation:
; #############################
;
; 1. Evaluate operator and _all_ operands.
; 2. Apply operator to operands (substitution).
;
; (expmod1 3 5 7)
; (remainder (* 3 (expmod1 3 4 7)) 7)
; (remainder (* 3 (remainder (square (expmod1 3 2 7)) 7)) 7)
; (remainder (* 3 (remainder (square (remainder (square (expmod1 3 1 7)) 7)) 7)) 7)
; (remainder (* 3 (remainder (square (remainder (square (remainder (* 3 (expmod1 3 0 7)) 7)) 7)) 7)) 7)
; (remainder (* 3 (remainder (square (remainder (square (remainder (* 3 1) 7)) 7)) 7)) 7)
; (remainder (* 3 (remainder (square (remainder (square (remainder 3 7)) 7)) 7)) 7)
; (remainder (* 3 (remainder (square (remainder (square 3) 7)) 7)) 7)
; (remainder (* 3 (remainder (square (remainder 9 7)) 7)) 7)
; (remainder (* 3 (remainder (square 2) 7)) 7)
; (remainder (* 3 (remainder 4 7)) 7)
; (remainder (* 3 4) 7)
; (remainder 12 7)
; 5
;
; (expmod2 3 5 7)
; (remainder (* 3 (expmod2 3 4 7)) 7)
; (remainder (* 3 (remainder (* (expmod 3 2 7) (expmod 3 2 7)) 7)) 7)
; (remainder (* 3 (remainder (* (remainder (* (expmod 3 1 7) (expmod 3 1 7)) 7) (remainder (* (expmod 3 1 7) (expmod 3 1 7)) 7)) 7)) 7)
; (remainder (* 3 (remainder (* (remainder (* (remainder (* 3 (expmod2 3 0 7)) 7) (remainder (* 3 (expmod2 3 0 7)) 7)) 7) (remainder (* (remainder (* 3 (expmod2 3 0 7)) 7) (remainder (* 3 (expmod2 3 0 7)) 7)) 7)) 7)) 7)
; (remainder (* 3 (remainder (* (remainder (* (remainder (* 3 1) 7) (remainder (* 3 1) 7)) 7) (remainder (* (remainder (* 3 1) 7) (remainder (* 3 1) 7)) 7)) 7)) 7)
; (remainder (* 3 (remainder (* (remainder (* (remainder 3 7) (remainder 3 7)) 7) (remainder (* (remainder 3 7) (remainder 3 7)) 7)) 7)) 7)
; (remainder (* 3 (remainder (* (remainder (* 3 3) 7) (remainder (* 3 3) 7)) 7)) 7)
; (remainder (* 3 (remainder (* (remainder 9 7) (remainder 9 7)) 7)) 7)
; (remainder (* 3 (remainder (* 2 2) 7)) 7)
; (remainder (* 3 (remainder 4 7)) 7)
; (remainder (* 3 4) 7)
; (remainder 12 7)
; 5
;
; By using explicit multiplication (*) instead of square proceedure, the amount
; of work done at each value of exp has doubled. Let exp have a positive integer
; value n. The work done at each level
;
; W(0) = k0
; W(n) = k1 + 2 * W(n/2)
;
;  i      Work Tree 
;  ################
;  0      k0
;
;  1      2 * k1 
;
;  2      4 * k1
;
;  ...
;
;  i      (2^i) * k1
;
;  Height = log n
;  Work_i = (2^i) * k1
;
;  Work = sum (0, (log n) - 1, k1 * (2^i))
;       = k0 + k1 * (1 - (2 ^ (log n))) / (1 - 2)             (geometric series)
;       = k0 + k1 * (2 ^ (log n)) - 1
;       = k0 + k1 * n
;  Work = O(n)

(expmod1 3 5 7)

(expmod2 3 5 7)
