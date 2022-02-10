(define (accumulate-rec combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate-rec combiner null-value term (next a) next b))))

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))

(define (sum term a next b)
  (accumulate-rec + 0 term a next b))

(define (product term a next b)
  (accumulate-iter * 1 term a next b))

(sum (lambda (x) x) 1 (lambda (x) (+ x 1)) 10)
(product (lambda (x) x) 1 (lambda (x) (+ x 1)) 6)
