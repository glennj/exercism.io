Class {
	#name : #Acronym,
	#superclass : #Object,
	#category : #'Exercise@Acronym'
}

{ #category : #transforming }
Acronym >> abbreviatePhrase: aString [
	^ String streamContents: [ :stream | 
		| state |
		state := #L.
		aString do: [ :char | 
			state = #L & char isLetter
				ifTrue: [ 
					stream nextPut: char asUppercase.
					state := #N ].
			state = #N & ((char isLetter) | (char = $')) not
				ifTrue: [ state := #L ] ] ]
]
