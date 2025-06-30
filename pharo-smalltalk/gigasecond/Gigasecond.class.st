"
Class Gigasecond
I can add one billion seconds to a datetime.
"
Class {
	#name : 'Gigasecond',
	#superclass : 'Object',
	#instVars : [
		'gigaSecond'
	],
	#category : 'Exercise@Gigasecond',
	#package : 'Exercise@Gigasecond'
}

{ #category : 'adding' }
Gigasecond >> addMoment: aDateTimeString [

	| future |
	future := (DateAndTime fromString: aDateTimeString) + gigaSecond.

	^ String streamContents: [ :stream | 
		future printYMDOn: stream.
		stream nextPut: $T.
		future printHMSOn: stream.
	]

]

{ #category : 'initialization' }
Gigasecond >> initialize [ 

	gigaSecond := Duration seconds: 1000000000.
	
]
