;; Ethan Johnston - CS 3210 - Spring 2016
;; factorial function
;; list processing functional language assignment
;; =============================================================================
;; returns the factorial of a given non-negative integer or NIL if the input is
;; negative or not an integer
;; =============================================================================
;; parameters:
;;     num - the integer whose factorial is returned
;; =============================================================================
;; limitations:
;;     0. none applicable
;; =============================================================================

( defun factorial ( num )
    ( cond
        ( ( or ( not ( integerp num ) ) ( < num 0 ) ) NIL )
        ( ( or ( = num 0 ) ( = num 1 ) ) 1 )
        ( T ( * num ( factorial ( - num 1 ) ) ) )
    )
)

;; test plan for factorial:
;; =============================================================================
;; category                           data           expected result
;; =============================================================================
;; non-number                         abc            NIL
;; non-integer number                 1.5            NIL
;; negative integer                   -1             NIL
;; zero                               0              1
;; one                                1              1
;; positive integer                   5              120
;; positive integer                   8              40320
