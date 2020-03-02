Class {
	#name : #Isogram,
	#superclass : #Object,
	#category : #'Exercise@Isogram'
}

{ #category : #'as yet unclassified' }
Isogram >> isIsogramPhrase: aString [
	| seen |
	seen := Bag new.
	(aString asLowercase select: #isLetter) do: [ :letter |
		(seen includes: letter) ifTrue: [ ^ false ].
		seen add: letter
	].
	^ true
]
