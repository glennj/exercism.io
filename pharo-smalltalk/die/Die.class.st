Class {
	#name : #Die,
	#superclass : #Object,
	#instVars : [
		'faces'
	],
	#category : #'Exercise@Die'
}

{ #category : #'as yet unclassified' }
Die class >> withFaces: anInteger [ 
	^ self basicNew setFaces: anInteger
]

{ #category : #acccessing }
Die >> faces [
	^faces
]

{ #category : #initialization }
Die >> initialize [
	"default die has 6 faces"
	faces := 6
]

{ #category : #action }
Die >> roll [
	^ faces atRandom
]

{ #category : #private }
Die >> setFaces: n [
	faces := n
]
