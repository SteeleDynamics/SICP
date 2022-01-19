(define (max-sum-squares a b c)
  (cond ((and (>= a b) (>= b c)) (+ (* a a) (* b b)))
        ((and (>= a c) (>= c b)) (+ (* a a) (* c c)))
        ((and (>= b a) (>= a c)) (+ (* b b) (* a a)))
        ((and (>= b c) (>= c a)) (+ (* b b) (* c c)))
        ((and (>= c a) (>= a b)) (+ (* c c) (* a a)))
        ((and (>= c b) (>= b a)) (+ (* c c) (* b b)))))

(max-sum-squares 0 1 2)

(max-sum-squares 4 3 5)

(max-sum-squares 8 6 7)

(max-sum-squares -1 -3 -2)
