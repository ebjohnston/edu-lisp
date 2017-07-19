;; Ethan Johnston - CS 3210 - Spring 2016
;; temp-convert function
;; list processing functional language assignment
;; =============================================================================
;; takes a temperature in the form of a number and unit (either Celsius or
;; Fahrenheit) and returns the temperature value converted to the other unit
;; =============================================================================
;; parameters:
;;     lst - the list containing a number and either "C" or "F"
;; =============================================================================
;; local functions:
;;     c-to-f: converts a number given in celsius to fahrenheit
;;     f-to-c: converts a number given in fahrenheit to celsius
;; =============================================================================
;; limitations:
;;     1. lst must be in the proper format (i.e. a number and unit)
;; =============================================================================

( defun temp-convert ( lst )
	( labels
		(
			( c-to-f( int-c ) ( + 32 ( * 1.8 int-c ) ) )
			( f-to-c( int-f ) ( / ( - int-f 32 ) 1.8 ) )
		)
		( cond
			( ( eq ( second lst ) 'C ) ( c-to-f ( first lst ) ) )
			( ( eq ( second lst ) 'F ) ( f-to-c ( first lst ) ) )
		)
	)
)

;; test plan for temp-convert:
;; =============================================================================
;; category                           data           expected result
;; =============================================================================
;; temperature in fahrenheit          212 F          100.0
;; temperature in fahrenheit          100 F          37.77778
;; temperature in celsius             100 C          212.0
;; temperature in celsius             212 C          413.59998
