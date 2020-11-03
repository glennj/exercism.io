"
Determine if a string is an isogram (each letter occurs exactly once).
"
Class {
	#name : #Isogram,
	#superclass : #Object,
	#category : #'Exercise@Isogram'
}

{ #category : #checking }
Isogram >> isIsogramPhrase: aString [ 
	| seen |
	seen := Set new.
	aString asLowercase
		select: #isLetter
		thenDo: [ :letter |
			seen
				addIfNotPresent: letter
				ifPresentDo: [ ^ false ]
		].
	^ true
]
