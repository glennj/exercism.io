Class {
	#name : #AtbashCipher,
	#superclass : #Object,
	#instVars : [
		'alphabet',
		'revAlphabet'
	],
	#category : #'Exercise@AtbashCipher'
}

{ #category : #encoding }
AtbashCipher >> decodePhrase: phrase [
	^ (phrase asLowercase asArray
		select: #isAlphaNumeric
		thenCollect: [ :c | revAlphabet at: (alphabet indexOf: c) ] )
		joinUsing: ''
]

{ #category : #encoding }
AtbashCipher >> encodePhrase: phrase [
	^ self grouped: (self decodePhrase: phrase)
]

{ #category : #private }
AtbashCipher >> grouped: phrase [
	^ (phrase allRegexMatches: '.{1,5}') joinUsing: ' '
]

{ #category : #initialization }
AtbashCipher >> initialize [
	| digits |
	alphabet := Character alphabet asOrderedCollection.
	revAlphabet := alphabet reversed.

	digits := $0 to: $9.
	alphabet addAll: digits.
	revAlphabet addAll: digits.
]
