;; Ethan Johnston - CS 3210 - Spring 2016
;; remove-negatives function
;; list processing functional language assignment
;; =============================================================================
;; takes as input a list of integers and returns a new list with all the
;; negative entries removed
;; =============================================================================
;; parameters:
;;     lst - the list of integers
;; =============================================================================
;; local functions:
;;     inner-remove-negatives - body of the recursion; used to generate
;;       interior list elements; assumes non-null initial input
;; =============================================================================
;; limitations:
;;     1. lst must contain only numbers
;;     2. lst must not contain nested lists
;; =============================================================================

( defun remove-negatives ( lst )
	( labels
		( ( inner-remove-negatives ( inner-lst )
			( cond
				( ( null inner-lst ) ( values ) )
				( ( and ( null ( rest inner-lst ) )
				  ( < ( first inner-lst ) 0 ) ) NIL )
				( ( < ( first inner-lst ) 0 )
				  ( inner-remove-negatives ( rest inner-lst ) ) )
				( T ( cons ( first inner-lst )
				  ( remove-negatives ( rest inner-lst ) ) ) )
			)
		) )
		( if ( null lst ) NIL ( inner-remove-negatives lst ) )
	)
)

;; test plan for remove-negatives:
;; =============================================================================
;; category                           data           expected result
;; =============================================================================
;; empty list                         ()             NIL
;; list of one positive number        (5)            (5)
;; list of one negative number        (-5)           NIL
;; list of all positive numbers       (1 2)          (1 2)
;; list of all negative numbers       (-1 -2 -3)     NIL
;; list of mixed-signed numbers       (-1 0 1 2)     (0 1 2)
