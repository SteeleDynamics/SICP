#|
 | Exercise 4.57
 |
 | Define a rule that says that person 1 can replace person 2 if either person 1
 | does the same job as person 2 or someone who does person 1's job can also do
 | person 2's job, and if person 1 and person 2 are not the same person. Using
 | your rule, give queries that find the following:
 | 
 | a. all people who can replace Cy D. Fect;
 |
 | b. all people who can replace someone who is being paid more than they are,
 | together with the two salaries.
 |
 | Answer
 | 
 | ... previous assertions
 |
 | (assert! (can-do-job (computer wizard) (conputer programmer)))
 | (assert! (can-do-job (computer wizard) (conputer technician)))
 | (assert! (can-do-job (computer programmer) (computer programmer trainee)))
 | (assert! (can-do-job (administration secretary) (administration big wheel)))
 |
 | (rule (same ?x ?x))
 |
 | (rule (can-replace ?person-1 ?person-2)
 |       (and (or (and (job ?person-1 ?same-job)
 |                     (job ?person-2 ?same-job))
 |                (and (job ?person-1 ?job-1)
 |                     (job ?person-2 ?job-2)
 |                     (can-do-job ?job-1 ?job-2)))
 |            (not (same ?person-1 ?person-2))))
 |
 | a. (can-replace ?person (Fect Cy D))
 |
 | b. (and (can-replace ?person-1 ?person-2)
 |         (salary ?person-1 ?salary-1)
 |         (salary ?person-2 ?salary-2)
 |         (lisp-value < ?salary-1 ?salary-2))
 |#
