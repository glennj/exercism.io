Class {
	#name : #SumOfMultiples,
	#superclass : #Object,
	#category : #'Exercise@SumOfMultiples'
}

{ #category : #calculating }
SumOfMultiples >> sumFactors: factorList limit: limit [ 
	| factors |
	factors := Set new.
	
	factorList do: [ :multiple |
		multiple > 0 ifTrue: [ 
		 	factors addAll: (multiple to: limit - 1 by: multiple) ] ].
	
	^ factors isEmpty
		ifTrue: [ 0 ]
		ifFalse: [ factors sum ]
]
