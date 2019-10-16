Class {
	#name : #Clock,
	#superclass : #Object,
	#instVars : [
		'minutes'
	],
	#category : #'Exercise@Clock'
}

{ #category : #'instance creation' }
Clock class >> hour: hour minute: minute [ 
	^ self new
		setMinutes: (hour * 60 + minute);
		yourself.
]

{ #category : #arithmetic }
Clock >> + durationInMinutes [
	self setMinutes: minutes + durationInMinutes
]

{ #category : #arithmetic }
Clock >> - durationInMinutes [
	self + durationInMinutes negated
]

{ #category : #comparing }
Clock >> = aClock [
	"note to self: when implementing `=`, implement `hash`"
	
	^ self hash = aClock hash
]

{ #category : #comparing }
Clock >> hash [
	^ minutes
]

{ #category : #initialization }
Clock >> initialize [
	super initialize.
	minutes := 0
]

{ #category : #printing }
Clock >> printOn: aStream [ 
	aStream
		nextPutAll: (minutes // 60) asTwoCharacterString;
		nextPut:    $:;
		nextPutAll: (minutes \\ 60) asTwoCharacterString
]

{ #category : #private }
Clock >> setMinutes: anInteger [
	"ensure the `minutes` value is between
	 - 0 ('00:00') and
	 - 1439 ('23:59') inclusive"
	
	minutes := anInteger \\ (24 * 60)
]
