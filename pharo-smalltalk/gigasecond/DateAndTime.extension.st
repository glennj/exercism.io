Extension { #name : #DateAndTime }

{ #category : '*Exercise@Gigasecond' }
DateAndTime >> asDateTimeString [
	^ String streamContents: [ :aStream |
		self printYMDOn: aStream.
		aStream nextPut: $T.
		self printHMSOn: aStream ]
]
