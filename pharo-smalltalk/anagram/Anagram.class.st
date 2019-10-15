Class {
	#name : #Anagram,
	#superclass : #Object,
	#category : #'Exercise@Anagram'
}

{ #category : #'searching and filtering' }
Anagram >> findAnagramsCandidates: aCollection subject: aString [ 
	| subject subjectKey lc |
	subject := aString asLowercase.
	subjectKey := subject sorted.
	
	^ aCollection select: [ :candidate | 
		lc := candidate asLowercase.
		lc ~= subject and: [ lc sorted = subjectKey ] ]
]
