Extension { #name : #Integer }

{ #category : #'*Exercise@CollatzConjecture' }
Integer >> nextCollatzNumber [
	^ self even
		ifTrue: [ self / 2 ]
		ifFalse: [ 3 * self + 1 ].
]
