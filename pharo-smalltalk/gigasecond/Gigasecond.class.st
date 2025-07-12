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

	^ ((DateAndTime fromString: aDateTimeString) + gigaSecond) asDateTimeString 


]

{ #category : 'initialization' }
Gigasecond >> initialize [ 

	gigaSecond := Duration seconds: 1000000000.
	
]
