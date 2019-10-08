Class {
	#name : #Isogram,
	#superclass : #Object,
	#category : #'Exercise@Isogram'
}

{ #category : #'as yet unclassified' }
Isogram >> isIsogramPhrase: aString [ 
	| seen lc |
	seen := Bag new.
	lc := aString asLowercase.
	1 to: (lc size) do: [ :idx |
		|char|
		char := lc at: idx.
		char isLetter ifTrue: [
			"short circuit: return immediately if a duplicate found"
			(seen includes: char) ifTrue: [ ^ false ].
			seen add: char ] ].
	^ true
]
