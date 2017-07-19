;; Ethan Johnston - CS 3210 - Spring 2016
;; count-div-five function
;; list processing functional language assignment
;; =============================================================================
;; checks a list of numbers and returns the amount of numbers divisible by five
;; =============================================================================
;; parameters:
;;     lst - the list of integers
;; =============================================================================
;; local functions:
;;     is-div-five - returns whether a number is divisible by five
;; =============================================================================
;; limitations:
;;     1. lst must contain only numbers
;;     2. lst must not contain nested lists
;; =============================================================================

( defun count-div-five ( lst )
    ( labels
      ( ( is-div-five ( tst ) ( = ( rem tst 5 ) 0 ) )
    )
    ( cond
      ( ( null lst ) 0 )
      ( ( is-div-five ( first lst ) )
        ( + 1 ( count-div-five ( rest lst ) ) ) )
      ( T ( count-div-five ( rest lst ) ) )
    )
  )
)

;; test plan for count-div-five:
;; =============================================================================
;; category                           data           expected result
;; =============================================================================
;; empty list                         ()             0
;; list of all divisible numbers      (5 10 15)      3
;; list of all indivisible numbers    (1 2 3 4)      0
;; list of numbers, some divisible    (1 5 7 10)     2
