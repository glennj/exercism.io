Class {
	#name : #Pangram,
	#superclass : #Object,
	#category : #'Exercise@Pangram'
}

{ #category : #testing }
Pangram >> isPangramSentence: sentence [
	^ sentence onlyLetters asLowercase asSet size = Character alphabet size
]
