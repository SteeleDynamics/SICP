;
; Exercise 2.76.
; ##############
; As a large system with generic operations evolves, new types of data objects
; or new operations may be needed. For each of the three strategies -- generic
; operations with explicit dispatch, data-directed style, and
; message-passing-style -- describe the changes that must be made to a system in
; order to add new types or new operations. Which organization would be most
; appropriate for a system in which new types must often be added? Which would
; be most appropriate for a system in which new operations must often be added?
;
; 1. Generic Operations with Explicit Dispatch:
;    - Adding new types means that predicates must be implemented in order to
;      detect which type the datum represents.
;    - All generic operations each individually dispatch to uniquely-named
;      procedures based on the predicate procedures.
;    - Adding new types means that new conditional clauses must dispatch to
;      newly-created, uniquely-named procedures that are the dispatch target.
;    - Addding new operations means we have to create a new generic operation
;      for each new operation.
;    - Each new generic operation is a dispatch function such that each existing
;      type must have its own clause in the conditional expression, a predicate
;      procedure to test the type of the datum, and a uniquely-named dispatch
;      target for each existing type as the consequent expression.
;
;    ==> Highly coupled code, not additive for new types or new operations
;
; 2. Data-Directed Style:
;    - Adding new types means that we create a new install-package procedure
;      which fills in the operation-table column for the new type by adding
;      a new local procedure to each generic operation row.
;    - Adding a new operation means we are adding a new row to operation-table,
;      where new local implementations of the new generic operation occupy each
;      column of the new row.
;
;    ==> Additive for new types (changes localized to new install-package proc)
;    ==> Requires implementation of operation-table
;    ==> Not additive for new ops (changes across all install-package procs)
;
; 3. Message-Passing Style:
;    - Adding a new type means creating a new constructor which returns a
;      dispatch function which captures the values of procedure params and
;      dispatches on an appropriate message.
;    - Adding a new operation to a type is simple because we construct a new
;      clause in the conditional expression in which the input message is the
;      new operation.
;
;    ==> Additive for new types and new operations
;    ==> Doesn't rely on operation table for dispatch, no install req'd
;    ==> All changes localized to dispatch function
;
;
; Both Data-Directed Style and Message-Passing Style are appropriate for a
; system in which new types must often be added. 
;
; However, only Message-Passing Style is appropriate for a system in which new
; operations must often be added.
;
