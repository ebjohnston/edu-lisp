;; Ethan Johnston - CS 3210 - Spring 2016
;; search-count founction
;; list processing functional language assignment
;; =============================================================================
;; searches through a list for a given integer and returns the number of
;; matches found in all lists, including embedded lists
;; =============================================================================
;; parameters:
;;     int - the number to be matched
;;     lst - the list of elements, possibly nested
;; =============================================================================
;; limitations:
;;     1. int must be a number
;; =============================================================================

( defun search-count ( int lst )
	( cond
		( ( null lst ) 0 )
		( ( listp ( first lst ) ) ( + ( search-count int ( first lst ) )
		  ( search-count int ( rest lst ) ) ) )
		( ( and ( numberp ( first lst ) ) ( = int ( first lst ) ) )
		  ( + 1 ( search-count int ( rest lst ) ) ) )
		( T ( search-count int ( rest lst ) ) )
	)
)

;; test plan for search-count:
;; =============================================================================
;; category                           data             expected result
;; =============================================================================
;; empty list                         1 ()             0
;; nonempty list, one match           1 (1)            1
;; nonempty list, one mismatch        1 (-1)           0
;; nonempty list, no matches          1 (a b 2 3)      0
;; nonempty list, all matches         1 (1 1 1)        3
;; nonempty list, some matches        1 (1 a b 2 1)    2
