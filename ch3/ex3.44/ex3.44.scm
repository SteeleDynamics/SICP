#|
 | Exercise 3.44
 |
 | Consider the problem of transferring an amount from one account to another.
 | Ben Bitdiddle claims that this can be accomplished with the following
 | procedure, even if there are multiple people concurrently transferring money
 | among multiple accounts, using any account mechanism that serializes deposit
 | and withdrawal transactions, for example, the version of make-account in the
 | text above.
 |
 | (define (transfer from-account to-account amount)
 |   ((from-account 'withdraw) amount)
 |   ((to-account 'deposit) amount))
 |
 | Louis Reasoner claims that there is a problem here, and that we need to use
 | a more sophisticated method, such as the one required for dealing with the
 | exchange problem. Is Louis right? If not, what is the essential difference
 | between the transfer problem and the exchange problem? (You should assume
 | that the balance in from-account is at least amount.)
 |
 |
 | Q1: Is Louis right?
 | A1: No.
 |
 | Q2: If not, what is the essential difference between the transfer problem and
 |     the exchange problem? (You should assume that the balance in from-account
 |     is at least amount.)
 | A2: The essential difference between the transfer problem and the exchange
 |     exchange problem is that the transfer problem limits the transferred
 |     amount to:
 |
 |       (from-account 'balance)
 |
 |     Whereas the exchange problem effectively increases the transferred amount
 |     to:
 |
 |       (max (from-account 'balance) (to-account 'balance)).
 |#
