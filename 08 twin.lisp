;; Ethan Johnston - CS 3210 - Spring 2016
;; twin function
;; list processing functional language assignment
;; =============================================================================
;; returns a list that duplicates each input element into a pair of adjacent
;; elements in the output list
;; =============================================================================
;; parameters:
;;     lst - the list of elements
;; =============================================================================
;; local functions:
;;     inner-twin - body of the recursion; used to generate interior elements;
;;       assumes non-null initial input
;; =============================================================================
;; limitations:
;;     1. lst must not contain nested lists
;; =============================================================================

( defun twin ( lst )
	( labels
		( ( inner-twin ( inner-lst )
			( cond
				( ( null inner-lst ) ( values ) )
				( T ( cons ( first inner-lst )
				  ( cons ( first inner-lst ) ( twin ( rest inner-lst ) ) ) ) )
			)
		) )
		( if ( null lst ) NIL ( inner-twin lst ) )
	)
)

;; test plan for twin:
;; =============================================================================
;; category                           data           expected result
;; =============================================================================
;; empty list                         ()             NIL
;; list of singular elements          (a b c)        (a a b b c c)
;; list of duplicate elements         (a a 1 1)      (a a a a 1 1 1 1)
;; list of elements, some duplicate   (1 1 2)        (1 1 1 1 2 2)
