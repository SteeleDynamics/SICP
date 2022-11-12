#|
 | Exercise 4.58
 |
 | Define a rule that says that a person is a "big shot" in a division if the
 | person works in the division but does not have a supervisor who works in the
 | division.
 |
 | Answer
 |
 | (rule (big-shot ?person ?division)
 |       (and (job ?person (?division . ?position))
 |            (or (not (supervisor ?person ?their-boss))
 |                (and (job ?their-boss (?their-division . ?their-position))
 |                     (not (same ?divison ?their-division))))
 |#
