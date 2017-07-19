;; Ethan Johnston - CS 3210 - Spring 2016
;; is-sorted function
;; list processing functional language asssignment
;; =============================================================================
;; returns whether a list of numbers is sorted in *strictly* ascending order
;; with no duplicate entries
;; =============================================================================
;; parameters:
;;     lst - the list of numbers to be checked for order
;; =============================================================================
;; limitations:
;;     1. lst must contain only numbers
;;     2. lst must not contain nested lists
;; =============================================================================

( defun is-sorted ( lst )
	( cond
		( ( null lst ) T )
		( ( null ( rest lst ) ) T )
		( ( < ( first lst ) ( second lst ) ) ( is-sorted ( rest lst ) ) )
		( T NIL )
	)
)

;; test plan for is-sorted:
;; =============================================================================
;; category                           data           expected result
;; =============================================================================
;; empty list                         ()             T
;; nonempty list, sorted              (1 2 5 10)     T
;; nonempty list, not sorted          (1 5 10 2)     NIL
;; nonempty list, reverse sorted      (4 3 2 1)      NIL
