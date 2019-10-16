Extension { #name : #Integer }

{ #category : #'*Exercise@Die' }
Integer >> d10 [
	"add specific die instead of overriding messageNotUnderstood:"
	^ self dieHandleForFaces: 10
]

{ #category : #'*Exercise@Die' }
Integer >> dieHandleForFaces: anInteger [ 
	^ DieHandle withNumDice: self ofFaces: anInteger
]

{ #category : #'*Exercise@Die' }
Integer >> d20 [
	"add specific die instead of overriding messageNotUnderstood:"
	^ self dieHandleForFaces: 20
]
