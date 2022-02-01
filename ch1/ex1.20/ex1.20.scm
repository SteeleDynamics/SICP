(define (gcd a b)
  (if (=  b 0)
      a
      (gcd b (remainder a b))))

;
; normal-order evaluation
;
; (gcd 206 40)
; (gcd 40 (remainder 206 40))
; (if (= (remainder 206 40) 0)
;     40
;     (gcd (remainder 206 40)
;          (remainder 40 (remainder 206 40))))
; (if (= 6 0)                                                   --> +1
;     40
;     (gcd (remainder 206 40)
;          (remainder 40 (remainder 206 40))))
; (gcd (remainder 206 40) (remainder 40 (remainder 206 40)))
; (if (= (remainder 40 (remainder 206 40)) 0)
;     (remainder 206 40)
;     (gcd (remainder 40 (remainder 206 40))
;          (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))
; (if (= 4 0)                                                   --> +2
;     (remainder 206 40)
;     (gcd (remainder 40 (remainder 206 40))
;          (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))
; (gcd (remainder 40 (remainder 206 40))
;      (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
; (if (= (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) 0)
;     (remainder 40 (remainder 206 40))
;     (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
;          (remainder (remainder 40 (remainder 206 40))
;                     (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))))
; (if (= 2 0)                                                   --> +4
;     (remainder 40 (remainder 206 40))
;     (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
;          (remainder (remainder 40 (remainder 206 40))
;                     (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))))
; (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
;      (remainder (remainder 40 (remainder 206 40))
;                 (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))
; (if (= (remainder (remainder 40 (remainder 206 40))
;                   (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
;        0)
;     (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
;     (gcd (remainder (remainder 40 (remainder 206 40))
;                     (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
;          (remainder (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
;                     (remainder (remainder 40 (remainder 206 40))
;                                (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))))
; (if (= 0 0)                                                   --> +7
;     (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
;     (gcd (remainder (remainder 40 (remainder 206 40))
;                     (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
;          (remainder (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
;                     (remainder (remainder 40 (remainder 206 40))
;                                (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))))
; (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
; 2                                                             --> +4
; ==> remainder operation was performed 18 times
;
;
; applicative-order evaluation
;
; (gcd 206 40)
; (gcd 40 (remainder 206 40))
; (gcd 40 6)                    --> +1
; (gcd 6 (remainder 40 6))
; (gcd 6 4)                     --> +1
; (gcd 4 (remainder 6 4))
; (gcd 4 2)                     --> +1
; (gcd 2 (remainder 4 2))
; (gcd 2 0)                     --> +1
; 2
; ==> remainder operation was performed 4 times

(gcd 206 40)
