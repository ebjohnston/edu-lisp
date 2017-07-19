;; Ethan Johnston - CS 3210 - Spring 2016
;; flatten function
;; list processing functional language assignment
;; =============================================================================
;; takes a list potentially containing nested lists and returns all embedded
;; elements as members of a new top-tier list with NIL (the empty list) not
;; being returned as an element of the new list
;; =============================================================================
;; parameters:
;;     lst - the list of elements, potentially nested
;; =============================================================================
;; local functions:
;;     inner-flatten - body of the recursion; used to generate interior list
;;       elements; assumes non-null initial input
;; =============================================================================
;; limitations:
;;     0. none applicable
;; =============================================================================

( defun flatten ( lst )
	( labels
		( ( inner-flatten ( inner-lst )
			( cond
				( ( null inner-lst ) ( values ) )
				( ( eq NIL ( first inner-lst ) )
				  ( append ( inner-flatten ( rest inner-lst ) ) ) )
				( ( atom ( first inner-lst ) ) ( cons ( first inner-lst )
				  ( inner-flatten ( rest inner-lst ) ) ) )
				( T ( append ( inner-flatten ( first inner-lst ) )
				  ( inner-flatten ( rest inner-lst ) ) ) )
			)
		) )
		( if ( null lst ) NIL ( inner-flatten lst ) )
	)
)

;; test plan for flatten:
;; =============================================================================
;; category                           data              expected result
;; =============================================================================
;; empty list                         ()                NIL
;; list of empty lists                ( () () )         NIL
;; list of top-tier elements          (a b c)           (a b c)
;; list, single-nested                (a (b c))         (a b c)
;; list, multi-nested                 (a () (b (c)))    (a b c)
