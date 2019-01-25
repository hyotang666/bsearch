(defpackage :bsearch.spec
  (:use :cl :jingoh #:bsearch))
(in-package :bsearch.spec)
(setup :bsearch)

(requirements-about BSEARCH)

;;;; Description:
; Do binary search.

#+syntax
(BSEARCH item vector
	 &key
	 (key #'identity)
	 (test #'eql)
	 (start 0)
	 (end (length vector))
	 (compare #'<)
	 (default nil)) ; => result 

;;;; Arguments and Values:

; item := T
; Target item to be found.
#?(bsearch 0 #(1 2 3)) => NIL
#?(bsearch 1 #(1 2 3)) => 1
#?(bsearch 2 #(1 2 3)) => 2
#?(bsearch 3 #(1 2 3)) => 3

; vector := simple vector, otherwise error.
#?(bsearch 0 '(1 2 3 4 5))
:signals (or type-error
	     warning) ; for ccl
#?(bsearch 0 (make-array 5 :initial-contents '(1 2 3 4 5)
			 :fill-pointer 5 :adjustable T))
=> unspecified
; Current version will signal an error (because of SVREF),
; but is work better? (with AREF.)

; key := function designator as (function(T)T), otherwise error.
#?(bsearch 0 #(1 2 3) :key :not-function-desinator)
:signals (or error
	     warning) ; for ccl
; The default is #'identity.
#?(bsearch 1 #("1" "2" "3")) :signals type-error
#?(bsearch 1 #("1" "2" "3") :key #'read-from-string)
=> "1"
,:test equal

; test := function designator as (function(T T)generarized-boolean), otherwise error.
#?(bsearch 0 #(1 2 3) :test :not-function-designator)
:signals (or error
	     warning) ; for ccl
; The default is #'eql.
#?(bsearch (princ-to-string 2) #("1" "2" "3")) :signals type-error
#?(bsearch (princ-to-string 2) #("1" "2" "3"):compare #'string<)
=> NIL
#?(bsearch (princ-to-string 2) #("1" "2" "3"):test #'equal :compare #'string<)
=> "2"
,:test equal

; start := (mod #.array-total-size-limit), otherwise error.
; Specify start position of VECTOR.
#?(bsearch 1 #(1 2 3) :start -1)
:signals error
#?(bsearch 1 #(1 2 3) :start 1)
=> NIL
#?(bsearch 1 #(1 2 3) :start 3)
:signals error
; The default is 0.

; end := (mot #.array-total-size-limit), otherwise error.
; Specify end position of VECTOR.
#?(bsearch 3 #(1 2 3) :end -1)
:signals error
#?(bsearch 3 #(1 2 3) :end 2)
=> NIL
#?(bsearch 3 #(1 2 3) :end 3)
=> 3
#?(bsearch 3 #(1 2 3) :end 4)
:signals error
; The default is `(length VECTOR)`.

; compare := function designator as (function(T T)generarized-boolean), otherwise error.
#?(bsearch 0 #(1 2 3) :compare :not-function-designator)
:signals (or error
	     warning) ; for ccl
#?(bsearch (princ-to-string 2) #("1" "2" "3"))
:signals type-error
#?(bsearch (princ-to-string 2) #("1" "2" "3"):compare #'string<)
=> NIL
#?(bsearch (princ-to-string 2) #("1" "2" "3"):compare #'string< :test #'string=)
=> "2"
,:test equal
; The default is #'<.

; default := T
; Return value when ITEM is not found.
#?(bsearch 0 #(1 2 3) :default :not-found)
=> :not-found

; result := (values found position)
; found := when found, found item, otherwise NIL.
; position := when found, its index, otherwise NIL.
#?(bsearch :a #(:a :b :c) :compare #'string<)
:values (:A 0)
#?(bsearch :b #(:a :b :c) :compare #'string<)
:values (:B 1)
#?(bsearch :c #(:a :b :c) :compare #'string<)
:values (:C 2)
#?(bsearch :d #(:a :b :c) :compare #'string<)
:values (NIL NIL)

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:
; VECTOR must sorted, otherwise unspecified.
#?(bsearch 0 #(1 3 2 5 4)) => unspecified

;;;; Exceptional-Situations:

