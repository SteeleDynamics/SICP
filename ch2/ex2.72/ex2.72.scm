;
; Exercise 2.72
; #############
; Consider the encoding procedure that you designed in exercise 2.68. What is
; the order of growth in the number of steps needed to encode a symbol? Be sure
; to include the number of steps needed to search the symbol list at each node
; encountered. To answer this question in general is difficult. Consider the
; special case where the relative frequencies of the n symbols are as described
; in exercise 2.71, and give the order of growth (as a function of n) of the
; number of steps needed to encode the most frequent and least frequent symbols
; in the alphabet.
;
; (define (encode-symbol x tree)
;   (cond ((leaf? tree) nil)
;         ((pair? (memq x (symbols (left-branch tree))))
;          (cons 0 (encode-symbol x (left-branch tree))))
;         (else (cons 1 (encode-symbol x (right-branch tree))))))
;
; Considering the special case where the relative frequencies of the n symbols
; are as described in exercise 2.71
;
; most frequent symbol:
; ---------------------
; 1. memq finds most freq symbol in left branch after 1 iter, O(1)
; 2. Evaluating consequent (cons 0 ...), O(1)
; 3. Subsequent recursion into top-level left branch is a leaf, O(1)
;
; ==> O(1)
;
; least frequent symbol:
; ----------------------
; 1. memq doesn't find least freq symbol in left branch after 1 iter, O(1)
; 2. Evaluating alternative (cons 1 ...), O(1)
; 3. Subsequent n-2 recursions
;
; ==> O(n)
;
