"
Implement a clock that handles times without dates.

You should be able to add and subtract minutes to it.

Two clocks that represent the same time should be equal to each other.


"
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
		minutes: (hour * 60 + minute);
		yourself.
]

{ #category : #arithmetic }
Clock >> + durationInMinutes [
	self minutes: minutes + durationInMinutes
]

{ #category : #arithmetic }
Clock >> - durationInMinutes [
	self + durationInMinutes negated
]

{ #category : #comparing }
Clock >> = aClock [
	"note to self: when implementing `=`, implement `hash`"
	^ self == aClock
		or: [ 
			self class = aClock class 
			and: [ self minutes = aClock minutes ] ]
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

{ #category : #accessing }
Clock >> minutes [
	^ minutes
]

{ #category : #private }
Clock >> minutes: anInteger [
	"ensure the `minutes` value is between
	 - 0 ('00:00') and
	 - 1439 ('23:59') inclusive"
	
	minutes := anInteger \\ (24 * 60)
]

{ #category : #printing }
Clock >> printOn: aStream [ 
	aStream
		nextPutAll: (minutes // 60) asTwoCharacterString;
		nextPut:    $:;
		nextPutAll: (minutes \\ 60) asTwoCharacterString
]
