"
I can take a string and reverse it.

Pedantically, I can take any instance of a SequenceableCollection

Example: ReverseString new reverseValue: 'hello world'  "" -> 'dlrow olleh""

"
Class {
	#name : #ReverseString,
	#superclass : #Object,
	#category : #'Exercise@ReverseString'
}

{ #category : #'as yet unclassified' }
ReverseString >> reverseValue: input [
	^ input species
		new: (input size)
		streamContents: [ :stream |
			input size to: 1 by: -1 do: [ :idx |
				stream nextPut: (input at: idx) ] ]
]
