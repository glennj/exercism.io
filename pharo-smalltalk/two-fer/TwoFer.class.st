Class {
	#name : #TwoFer,
	#superclass : #Object,
	#category : #'Exercise@TwoFer'
}

{ #category : #'as yet unclassified' }
TwoFer >> who [
	^ self who: 'you'.
]

{ #category : #'as yet unclassified' }
TwoFer >> who: name [ 
	name ifNil: [ ^ self who ].
	^ 'One for {1}, one for me.' format: {name}.
]
