"
I will return a message containing the given name, or a default value if no name given. 
"
Class {
	#name : #TwoFer,
	#superclass : #Object,
	#category : #'Exercise@TwoFer'
}

{ #category : #translating }
TwoFer >> who [
	^ self who: 'you'.
]

{ #category : #translating }
TwoFer >> who: name [ 
	^ name
		ifNil: [ self who ]
		ifNotNil: [ 'One for {1}, one for me.' format: {name} ].
]
