Class {
	#name : #Raindrops,
	#superclass : #Object,
	#category : #'Exercise@Raindrops'
}

{ #category : #private }
Raindrops class >> soundsDictionary [
	^ {
		3 -> 'Pling' .
		5 -> 'Plang' .
		7 -> 'Plong' } asOrderedDictionary
]

{ #category : #converting }
Raindrops >> convertNumber: anInteger [ 
	^ String streamContents: [ :stream |
		self class soundsDictionary
			keysAndValuesDo: [ :divisor :sound |
				(anInteger isDivisibleBy: divisor)
					ifTrue: [ stream nextPutAll: sound ] ].
		stream isEmpty
			ifTrue: [ stream nextPutAll: anInteger asString ] ]
]
