; if  b > 0, then select + operator, else select - operator
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

(a-plus-abs-b 2 3)

(a-plus-abs-b 4 -5)
