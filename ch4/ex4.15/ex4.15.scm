#|
 | Exercise 4.15
 |
 | Given a one-argument procedure 'p' and an object 'a', 'p' is said to "halt"
 | on 'a' if evaluating the expression '(p a)' returns a value (as opposed to
 | terminating with an error message or running forever). Show that it is
 | impossible to write a procedure 'halts?' that correctly determines whether
 | 'p' halts on 'a' for any procedure 'p' and object 'a'. Use the following
 | reasoning: If you had such a procedure 'halts?', you could implement the
 | following program:
 |
 | (define (run-forever) (run-forever))
 |
 | (define (try p)
 |   (if (halts? p p)
 |       (run-forever)
 |       'halted))
 |
 | Now consider evaluating the expression '(try try)' and show that any
 | possible outcome (either halting or running forever) violates the intended
 | behavior of 'halts?'.
 |
 | Answer
 |
 | (Proof by Contradiction)
 |
 | Case 1: Assume (try try) returns a value. Then (halts? try try) evaluates to
 | true. According to the definition of the try procedure:
 |
 |     (try try)
 | ==> (if (halts? try try) (run-forever) 'halted))                   (defn try)
 | ==> (run-forever)                                (Case 1 assumption, if expr)
 | ==> (run-forever)                                          (defn run-forever)
 | ==> ...
 |
 | We see that the expression (try try) doesn't return a value. But our
 | assumption was that (try try) does return a value. This is a contradiction.
 |
 | Case 2: Assume (try try) doesn't return a value. Then (halts? try try)
 | evaluates to false. According to the definition of the try procedure:
 |
 |     (try try)
 | ==> (if (halts? try try) (run-forever) 'halted))                   (defn try)
 | ==> 'halted                                      (Case 2 assumption, if expr)
 |
 | We see that the expression (try try) returns a value. But our assumption was
 | that (try try) doesn't return a value. This is a contradiction.
 |
 | Therefore, it is impossible to write a procedure 'halts?' that correctly
 | determines whether 'p' halts on 'a' for any procedure 'p' and object 'a'.
 |
 | Q.E.D.
 |#
