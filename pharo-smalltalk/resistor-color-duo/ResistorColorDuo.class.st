Class {
	#name : #ResistorColorDuo,
	#superclass : #Object,
	#category : #'Exercise@ResistorColorDuo'
}

{ #category : #'as yet unclassified' }
ResistorColorDuo class >> resistorColors [
	^ #(
		'black' 'brown' 'red' 'orange' 'yellow'
		'green' 'blue' 'violet' 'grey' 'white'
	)
]

{ #category : #accessing }
ResistorColorDuo >> valueOf: color [
	| idx |
	idx := self class resistorColors
		indexOf: color 
		ifAbsent: [ self error: ('Invalid color {1}' format: {color}) ].
	^ idx - 1

]

{ #category : #'as yet unclassified' }
ResistorColorDuo >> valueWithColors: colors [ 
	^ (colors first: 2)
		inject: 0 into: [ :resistorValue :color |
			10 * resistorValue + (self valueOf: color) ]
]
