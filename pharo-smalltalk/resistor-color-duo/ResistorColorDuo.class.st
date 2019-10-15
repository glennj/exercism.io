Class {
	#name : #ResistorColorDuo,
	#superclass : #Object,
	#category : #'Exercise@ResistorColorDuo'
}

{ #category : #accessing }
ResistorColorDuo >> valueOf: color [
	| idx |
	idx := 
		#( 'black' 'brown' 'red' 'orange' 'yellow'
			'green' 'blue' 'violet' 'grey' 'white' )
				indexOf: color 
				ifAbsent: [ self error: ('invalid color ', color) ].
	"index of 'black' is 1, but the _value_ is 0"
	^ idx - 1

]

{ #category : #converting }
ResistorColorDuo >> valueWithColors: colors [ 
	^ (colors first: 2)
		inject: 0 into: [ :resistorValue :color |
			10 * resistorValue + (self valueOf: color) ]
]
