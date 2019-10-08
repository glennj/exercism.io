Class {
	#name : #Grains,
	#superclass : #Object,
	#category : #'Exercise@Grains'
}

{ #category : #accessing }
Grains >> atSquare: n [
	(n between: 1 and: 64)
		ifFalse: [ self error: 'square must be between 1 and 64' ].
	^ 2 ** (n - 1)
]

{ #category : #accessing }
Grains >> total [
	^ (2 ** 64) - 1
]
