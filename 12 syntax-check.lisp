;; Ethan Johnston - CS 3210 - Spring 2016
;; check-syntax function
;; list processing functional language assignment
;; =============================================================================
;; checks whether a list is a valid expression of the form number-operator-
;; number with number being an integer and operator being one of the following:
;; plus, minus, times, or dividedby; can contain nested expressions on either
;; side
;; =============================================================================
;; parameters:
;;     lst - list containing an expresion, potentially invalid
;; =============================================================================
;; local functions:
;;     check-tokens - ensures a list has three top-tier elements
;;     check-operator - ensures the middle element is a valid operator
;;     check-operands - ensures the first and third elements are integers or
;;       lists; if an element is a list, it calls the main function recursively
;; =============================================================================
;; limitations:
;;     0. none applicable
;; =============================================================================

( defun check-syntax ( lst )
	( labels
		(
			( check-tokens ( token-lst )
				( cond
					( ( null token-lst ) NIL )
					( ( null ( rest token-lst ) ) NIL )
					( ( null ( rest ( rest token-lst ) ) ) NIL )
					( ( null ( rest ( rest ( rest token-lst ) ) ) ) T )
					( T NIL )
				)
			)
			( check-operator ( operator-lst )
				( cond
					( ( eq 'plus ( second operator-lst ) ) T )
					( ( eq 'minus ( second operator-lst ) ) T )
					( ( eq 'times ( second operator-lst ) ) T )
					( ( eq 'dividedby ( second operator-lst ) ) T )
					( T NIL )
				)
			)
			( check-operands ( operands-lst )
				( cond
					( ( and ( listp ( first operands-lst ) ) ( listp ( third operands-lst ) ) )
					  ( and ( check-syntax ( first operands-lst ) ) ( check-syntax ( third operands-lst ) ) ) )
					( ( listp ( first operands-lst ) )
					  ( and ( check-syntax ( first operands-lst ) ) ( integerp ( third operands-lst ) ) ) )
					( ( listp ( third operands-lst ) )
					  ( and ( integerp ( first operands-lst ) ) ( check-syntax ( third operands-lst ) ) ) )
					( T ( and ( integerp ( first operands-lst ) ) ( integerp ( third operands-lst ) ) ) )
				)
			)
		)
		( cond
			( ( null lst ) NIL )
			( T ( and ( check-tokens lst ) ( check-operator lst ) ( check-operands lst ) ) )
		)
	)
)

;; test plan for syntax-check:
;; =============================================================================
;; category                       data                           expected result
;; =============================================================================
;; empty list                     ()                             NIL
;; simple expression              (1 plus 2)                     T
;; simple, wrong operator         (1 + 2)                        NIL
;; simple, wrong operand          (a plus b)                     NIL
;; nested expression              (1 plus (2 minus 3))           T
;; nested, wrong operator         (1 plus (2 - 3))               NIL
;; nested, wrong operand          (1 plus (a minus b))           NIL
;; double-nested expression       ((1 plus 2) minus (2 plus 3))  T
;; double-nested, wrong operator  ((1 plus 2) minus (2 + 3))     NIL
;; double-nested, wrong operand   ((a plus b) minus (2 plus 3))  NIL
;; multi-nested expression        (((3 plus 2) minus 4) plus 1)  T
;; multi-nested, wrong operator   (((3 + 2) minus 4) plus 1)     NIL
;; multi-nested, wrong operand    (((a plus b) minus 4) plus 1)  NIL
