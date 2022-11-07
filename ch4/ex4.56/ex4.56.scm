#|
 | Exercise 4.56
 |
 | Formulate compound queries that retrieve the following information:
 |
 | a. the names of all people who are supervised by Ben Bitdiddle, together with
 |    their addresses;
 | b. all people whose salary is less than Ben Bitdiddle's, together with their
 |    salary and Ben Bitdiddle's salary;
 | c. all people who are supervised by someone who is not in the computer
 |    division, together with the supervisor's name and job.
 |
 | Answer
 |
 | a. (and (supervisor ?x (Bitdiddle Ben)) (address ?x ?y))
 | b. (and (salary (Bitdiddle Ben) ?a) (salary ?x ?b) (lisp-value < ?b ?a))
 | c. (and (supervisor ?w ?x) (not (job ?x (computer . ?y))) (job ?x ?z))
 |#
