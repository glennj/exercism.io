Extension { #name : #Character }

{ #category : #'*Exercise@IsbnVerifier' }
Character >> isIsbnCharacter [

	^ self isDigit or: [ self = $X ]
]

{ #category : #'*Exercise@IsbnVerifier' }
Character >> isbnCharacterValue [

	^ self = $X ifTrue: [ 10 ] ifFalse: [ self digitValue ]
]
