Extension { #name : #Integer }

{ #category : #'*Exercise@CollatzConjecture' }
Integer >> nextCollatzNumber [
	^ (self isDivisibleBy: 2)
		ifTrue: [ self / 2 ]
		ifFalse: [ 3 * self + 1 ].
]
