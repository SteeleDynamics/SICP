; random-in-range procedure
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

; estimate-integral procedure
(define (estimate-integral P x1 x2 y1 y2 n)
  (define (experiment)
    (P (random-in-range x1 x2)
       (random-in-range y1 y2)))
  (let ((ratio (monte-carlo n experiment))
        (area (* (- x2 x1) (- y2 y1))))
    (* ratio area)))

; monte-carlo procedure
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

; unit-circle? predicate procedure
(define (unit-circle? x y)
  (<= (+ (square x) (square y)) 1.0))

; rectangular bounds
(define x1 -2.0)
(define x2 2.0)
(define y1 -1.25)
(define y2 1.25)

; number of trials
(define n 10000000)

; estimate pi
(estimate-integral unit-circle? x1 x2 y1 y2 n)
