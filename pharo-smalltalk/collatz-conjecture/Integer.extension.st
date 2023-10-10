Extension { #name : #Integer }

{ #category : #'*Exercise@CollatzConjecture' }
Integer >> collatzSteps [

	^ self = 1
		ifTrue: [ 0 ]
		ifFalse: [ self nextCollatzNumber collatzSteps + 1 ]
]

{ #category : #'*Exercise@CollatzConjecture' }
Integer >> nextCollatzNumber [
	^ self even
		ifTrue: [ self / 2 ]
		ifFalse: [ 3 * self + 1 ].
]
