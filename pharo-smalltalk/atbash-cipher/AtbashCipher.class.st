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
	^ self xxcode: phrase
]

{ #category : #encoding }
AtbashCipher >> encodePhrase: phrase [
	^ self grouped: (self xxcode: phrase)
]

{ #category : #private }
AtbashCipher >> grouped: phrase [
	^ (phrase allRegexMatches: '.{1,5}') joinUsing: ' '
]

{ #category : #initialization }
AtbashCipher >> initialize [
	alphabet := Character alphabet asOrderedCollection.
	revAlphabet := alphabet reversed.
	
	alphabet addAll: ($0 to: $9).
	revAlphabet addAll: ($0 to: $9).
]

{ #category : #private }
AtbashCipher >> xxcode: phrase [
	^ (phrase asLowercase asArray
		select: [ :c | c isAlphaNumeric ]
		thenCollect: [ :c | revAlphabet at: (alphabet indexOf: c) ] )
		joinUsing: ''
]
