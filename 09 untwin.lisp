;; Ethan Johnston - CS 3210 - Spring 2016
;; untwin function
;; list processing functional language assignment
;; =============================================================================
;; returns a list that removes an element if it is idential to the element
;; directly previous in a list with the comparison done in pairs of two
;; (maximum of one deletion per pair)
;; =============================================================================
;; parameters:
;;     lst - the list of elements
;; =============================================================================
;; local functions:
;;     inner-untwin - body of the recursion; used to generate interior
;;       elements; assumes non-null initial input
;; =============================================================================
;; limitations:
;;     1. lst must not contain nested lists
;; =============================================================================

( defun untwin ( lst )
	( labels
		( ( inner-untwin ( inner-lst )
			( cond
				( ( null inner-lst ) ( values ) )
				( ( null ( rest inner-lst ) )
				  ( cons ( first inner-lst ) ( values ) ) )
				( ( eq ( first inner-lst ) ( second inner-lst ) )
				  ( cons ( first inner-lst ) ( untwin ( rest ( rest inner-lst ) ) ) ) )
				( T ( cons ( first inner-lst ) ( untwin ( rest inner-lst ) ) ) )
			)
		) )
		( if ( null lst ) NIL ( inner-untwin lst ) )
	)
)

;; test plan for untwin:
;; =============================================================================
;; category                           data           expected result
;; =============================================================================
;; empty list                         ()             NIL
;; list of singular elements          (a b c)        (a b c)
;; list of duplicate elements         (a a 1 1)      (a 1)
;; list of elements, some duplicate   (1 1 1 2)      (1 1 2)
