;
; Eva Lu Ator is correct. Using algebraically equivalent expressions containing
; the minimum number of intervals will always produce the tightest bounds
; (smallest percentages). To illustrate this fact, output from ex2.14 for the
; expression A / A yields:
;
; (define A (make-center-percent 23.5 0.07))
; Value: a
;
; 1 ]=> (define res3 (div-interval A A))
; Value: res3
;
; 1 ]=> (center res3)
; Value: 1.00000098000048
;
; 1 ]=> (percent res3)
; Value: .1399999314000158
;
; From ex2.13, we saw that under the assumption of small percentages, the
; percentage of the product interval was approximately equal to the sum of the
; multiplicand and multiplier percentages. Since interval division is
; implemented using interval multiplication, the same small interval assumption
; also applies. This is counter-intuitive to the concept of an identity
; interval for multiplication and division:
;
; 1 ]=> (define one (make-center-percent 1 0))
; Value: one
;
; 1 ]=> (define res5 (mul-interval A one))
; Value: res5
;
; 1 ]=> (center res5)
; Value: 23.5
;
; 1 ]=> (percent res5)
; Value: .0699999999999956
;
; 1 ]=> (define res6 (div-interval A one))
; Value: res6
;
; 1 ]=> (center res6)
; Value: 23.5
;
; 1 ]=> (percent res6)
; Value: .0699999999999956
;
; We can now see that the expression A / A != one. In other words, algebraically
; equivalent expressions are not equivalent when using interval arithmetic.
;
