;; Ethan Johnston - CS 3210 - Spring 2016
;; move-to-front function
;; list processing functional language assignment
;; =============================================================================
;; checks to see if a word is in a list; if yes, the word is moved to the front
;; of the list; if no, the word is appended to the front of the list
;; =============================================================================
;; parameters:
;;     wrd - the word to be moved or added
;;     lst - the list to be searched for the word
;; =============================================================================
;; local functions:
;;     is-found - returns whether a word is an element of a list
;;     remove-wrd - returns a list with a word removed, if present
;; =============================================================================
;; limitations:
;;     1. wrd must be a single element
;;     2. lst must not contain nested lists
;;     3. wrd must occur no more than once in lst
;; =============================================================================

( defun move-to-front ( wrd lst )
  ( labels
    (
      ( is-found ( search-wrd search-lst )
        ( cond
          ( ( null search-lst ) NIL )
          ( ( eq search-wrd ( first search-lst ) ) T )
          ( T ( is-found search-wrd ( rest search-lst ) ) )
        )
      )
      ( remove-wrd ( removed-wrd remove-lst )
        ( cond
          ( ( and ( null ( rest remove-lst ) )
            ( eq removed-wrd ( first remove-lst ) ) ) ( values ) )
          ( ( null ( rest remove-lst ) ) ( first remove-lst ) )
          ( ( eq removed-wrd ( first remove-lst ) )
            ( rest remove-lst ) )
          ( T ( cons ( first remove-lst )
            ( remove-wrd removed-wrd ( rest remove-lst ) ) ) )
        )
      )
    )
    ( cond
      ( ( null lst ) ( list wrd ) )
      ( ( not ( is-found wrd lst ) ) ( cons wrd lst ) )
      ( ( eq wrd ( first lst ) ) lst )
      ( T ( cons wrd ( remove-wrd wrd lst ) ) )
    )
  )
)

;; test plan for move-to-front:
;; =============================================================================
;; category                           data           expected result
;; =============================================================================
;; empty list                         a ()           (a)
;; list not containing word           a (x y)        (a x y)
;; list containing word, not first    a (b c d a)    (a b c d)
;; list containing word, first        a (a b c)      (a b c)
