Class {
	#name : #Hamming,
	#superclass : #Object,
	#category : #'Exercise@Hamming'
}

{ #category : #private }
Hamming >> distanceFor: left and: right withCount: count [
	left isEmpty ifTrue: [ ^ count ].
	^ self 
		distanceFor: left allButFirst 
		and: right allButFirst 
		withCount: count + (left first = right first ifTrue: [ 0 ] ifFalse: [ 1 ])
]

{ #category : #calculating }
Hamming >> distanceStrand1: leftStrand strand2: rightStrand [
	leftStrand size = rightStrand size
		ifFalse: [ 
			leftStrand  isEmpty ifTrue: [ self error: 'left strand must not be empty' ].
			rightStrand isEmpty ifTrue: [ self error: 'right strand must not be empty' ].
			self error: 'left and right strands must be of equal length' ].
		
	" first take: a recursive helper:
	^ self distanceFor: leftStrand and: rightStrand withCount: 0
	"
	
	" next: iterate through the strands in parallel
	^ ((leftStrand asArray 
			with: rightStrand asArray 
			collect: [ :left :right | left = right ])
		reject: [ :isEqual | isEqual ])
		size
	"
	
	" finally: discover the magic method "
	^ leftStrand size - (leftStrand howManyMatch: rightStrand)
]
