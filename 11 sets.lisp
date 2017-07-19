;; Ethan Johnston - CS 3210 - Spring 2016
;; set functions - member, union, and intersection
;; list processing functional language assignment
;; =============================================================================
;; main functions:
;;     set-member - returns whether a word is an element of a given list
;;     set-union - returns the union of two sets as a new, combined set
;;     set-intersect - returns the intersection of two sets as a new set
;; =============================================================================
;; parameters:
;;     wrd - word for which the list is searched
;;     lst - list of elements to be searched
;;     lst-one - the first list to be joined
;;     lst-two - the second list to be joined
;; =============================================================================
;; local functions:
;;     remove-word - returns a list with a word removed, if present
;;     inner-union - body of the recursion; used to generate
;;       interior list elements; assumes non-null initial input(s)
;;     inner-intersect - body of the recursion; used to generate
;;       interior list elements; assumes non-null initial input(s)
;; =============================================================================
;; limitations:
;;     1. the lists must not contain nested lists
;;     2. wrd must be a single element
;; =============================================================================

( defun remove-word ( removed-wrd remove-lst )
	( cond
		( ( and ( null ( rest remove-lst ) )
		  ( eq removed-wrd ( first remove-lst ) ) ) ( values ) )
		( ( null ( rest remove-lst ) ) ( first remove-lst ) )
		( ( eq removed-wrd ( first remove-lst ) ) ( rest remove-lst ) )
		( T ( cons ( first remove-lst )
		  ( remove-word removed-wrd ( rest remove-lst ) ) ) )
	)
)

( defun set-member ( wrd lst )
	( cond
		( ( null lst ) NIL )
		( ( eq wrd ( first lst ) ) T )
		( T ( set-member wrd ( rest lst ) ) )
	)
)

;; test plan for set-member:
;; =================================================================
;; category                           data           expected result
;; =================================================================
;; empty list                         a ()           NIL
;; list does not contain word         a (b c d)      NIL
;; list does contain word             a (a b c)      T
;; list contains word, nested         a ((a) b c)    NIL

( defun set-union ( lst-one lst-two )
	( labels
		(
			( inner-union ( inner-one inner-two )
				( cond
					( ( and ( null inner-one ) ( null inner-two ) ) ( values ) )
					( ( null inner-one ) ( append inner-two ) )
					( ( null inner-two ) ( append inner-one ) )
					( ( set-member ( first inner-one ) inner-two )
					  ( cons ( first inner-one ) ( inner-union ( rest inner-one )
						( remove-word ( first inner-one ) inner-two ) ) ) )
					( ( set-member ( first inner-two ) inner-one )
					  ( cons ( first inner-two ) ( inner-union ( rest inner-two )
						( remove-word ( first inner-two ) inner-one ) ) ) )
					( T ( cons ( first inner-one ) ( cons ( first inner-two )
					  ( inner-union ( rest inner-one ) ( rest inner-two ) ) ) ) )
				)
			)
		)
		( cond
			( ( and ( null lst-one ) ( null lst-two ) ) NIL )
			( ( null lst-one ) lst-two )
			( ( null lst-two ) lst-one )
			( T ( inner-union lst-one lst-two ) )
		)
	)
)

;; test plan for set-union:
;; =============================================================================
;; category                           data           expected result
;; =============================================================================
;; both empty lists                   () ()          NIL
;; one empty list                     () (a b)       (a b)
;; other empty list                   (a b) ()       (a b)
;; identical lists                    (b c) (b c)    (b c)
;; distinct lists                     (a b) (c d)    (a b c d)
;; overlapping lists                  (a b) (b c)    (a b c)

( defun set-intersect ( lst-one lst-two )
	( labels
		(
			( inner-intersect ( inner-one inner-two )
				( cond
					( ( or ( null inner-one ) ( null inner-two ) ) ( values ) )
					( ( set-member ( first inner-one ) inner-two )
					  ( cons ( first inner-one ) ( inner-intersect ( rest inner-one )
						( remove-word ( first inner-one ) inner-two ) ) ) )
					( ( set-member ( first inner-two ) inner-one )
					  ( cons ( first inner-two ) ( inner-intersect ( rest inner-two )
						( remove-word ( first inner-two ) inner-one ) ) ) )
					( T ( append ( inner-intersect ( rest inner-one )
					  ( rest inner-two ) ) ) )
				)
			)
		)
		( cond
			( ( or ( null lst-one ) ( null lst-two ) ) NIL )
			( T ( inner-intersect lst-one lst-two ) )
		)
	)
)

;; test plan for set-intersect:
;; =================================================================
;; category                           data           expected result
;; =================================================================
;; both empty lists                   () ()          NIL
;; one empty list                     () (a b)       NIL
;; other empty list                   (a b) ()       NIL
;; identical lists                    (b c) (b c)    (b c)
;; distinct lists                     (a b) (c d)    NIL
;; overlapping lists                  (a b) (b c)    (b)
