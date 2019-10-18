Class {
	#name : #MatchingBrackets,
	#superclass : #Object,
	#instVars : [
		'brackets'
	],
	#category : #'Exercise@MatchingBrackets'
}

{ #category : #initialization }
MatchingBrackets >> initialize [
	brackets := { $} -> ${ . $) -> $( . $] -> $[ } asDictionary.
]

{ #category : #testing }
MatchingBrackets >> isCloseBracket: char [
	^ brackets includesKey: char
]

{ #category : #private }
MatchingBrackets >> isMatchingBracket: char in: stack [
	^ stack isNotEmpty and: [ (brackets at: char) = (stack pop) ]
]

{ #category : #testing }
MatchingBrackets >> isOpenBracket: char [
	^ brackets includes: char
]

{ #category : #testing }
MatchingBrackets >> isPaired: aString [ 
	| stack |
	stack := Stack new.
	aString asArray do: [ :char |
		(self isOpenBracket: char)
			ifTrue: [ stack push: char ]
			ifFalse: [ 
				((self isCloseBracket: char) and: [
					(self isMatchingBracket: char in: stack) not ])
						ifTrue: [ ^ false ] ] ].
	^ stack isEmpty
]
