Class {
	#name : #ReverseString,
	#superclass : #Object,
	#category : #'Exercise@ReverseString'
}

{ #category : #'as yet unclassified' }
ReverseString >> reverseValue: aString [
	^ String
		new: (aString size)
		streamContents: [ :stream |
			aString size to: 1 by: -1 do: [ :idx |
				stream nextPut: (aString at: idx) ] ]
]
