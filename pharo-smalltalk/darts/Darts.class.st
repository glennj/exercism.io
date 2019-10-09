Class {
	#name : #Darts,
	#superclass : #Object,
	#category : #'Exercise@Darts'
}

{ #category : #calculating }
Darts >> scoreX: x y: y [ 
	| dist |
	dist := ((x squared) + (y squared)).

	dist <=   1          ifTrue: [ ^ 10 ]. 
	dist <= ( 5 squared) ifTrue: [ ^  5 ]. 
	dist <= (10 squared) ifTrue: [ ^  1 ].
	^ 0
]
