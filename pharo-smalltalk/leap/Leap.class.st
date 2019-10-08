"
Determine if a given year is a leap year.
"
Class {
	#name : #Leap,
	#superclass : #Object,
	#category : #'Exercise@Leap'
}

{ #category : #'as yet unclassified' }
Leap >> leapYear: year [ 
   "this is a leap year if:
    - it is divisible by 4, and
    - is is _not_ divisible by 100 or it is divisible by 400
   "

	^ (year \\ 4 = 0) and: ((year \\ 100 = 0) not or: (year \\ 400 = 0)).
]
