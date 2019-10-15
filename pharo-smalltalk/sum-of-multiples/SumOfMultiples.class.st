Class {
	#name : #SumOfMultiples,
	#superclass : #Object,
	#category : #'Exercise@SumOfMultiples'
}

{ #category : #calculating }
SumOfMultiples >> sumFactors: aCollection limit: anInteger [ 
	| factors |
	factors := Set new.
	
	aCollection do: [ :multiple |
		multiple > 0
			ifTrue: [ 
				factors addAll: (multiple to: anInteger - 1 by: multiple) ] ].
	
	^ factors isEmpty
		ifTrue: [ 0 ]
		ifFalse: [ factors sum ]
]
