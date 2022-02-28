;
; Original queens procedure:
; ##########################
;
; (define (queens board-size)
;   (define (queen-cols k)
;     (if (= k 0)
;         (list empty-board)
;         (filter
;          (lambda (positions) (safe? k positions))
;          (flatmap
;           (lambda (rest-of-queens)
;             (map (lambda (new-row)
;                    (adjoin-position new-row k rest-of-queens))
;                  (enumerate-interval 1 board-size)))
;           (queen-cols (- k 1))))))
;   (queen-cols board-size))
;
;   Let board-size = n, then:
;
;   k     T(k) for (queen-cols k)
;   -----------------------------
;   0     1
;   1     n * T(0)
;   2     n * T(1)
;   3     n * T(2)
;   4     n * T(3)
;   ...
;   k     n * T(k-1)
;   -----------------------------
;   T(n) = n*T(n-1) + n*T(n-2) + ... + 1
;   
;   (queens n) --> O(n^n)
;
;
; Modified queens procedure:
; ##########################
;
; (define (queens board-size)
;   (define (queen-cols k)
;     (if (= k 0)
;         (list empty-board)
;         (filter
;          (lambda (positions) (safe? k positions))
;          (flatmap
;           (lambda (rest-of-queens)
;             (map (lambda (new-row)
;                    (adjoin-position new-row k rest-of-queens))
;                  (queen-cols (- k 1))))
;           (enumerate-interval 1 board-size)))))
;   (queen-cols board-size))
;
;   Let board-size = n, then:
;
;   k     T(k) for (queen-cols k)
;   -----------------------------
;   0     1
;   1     n ^ T(0)
;   2     n ^ T(1)
;   3     n ^ T(2)
;   4     n ^ T(3)
;   ...
;   k     n ^ T(k-1)
;   -----------------------------
;   T(n) = n^T(n-1) + n^T(n-2) + ... + 1
;
;   (queens n) --> O(n^n^...^n) (n-times)
;
