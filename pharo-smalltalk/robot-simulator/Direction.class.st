"
I represent the Cardinal direction of a Robot: north, east, south, west.
"
Class {
	#name : #Direction,
	#superclass : #Object,
	#instVars : [
		'direction'
	],
	#category : #'Exercise@RobotSimulator'
}

{ #category : #constants }
Direction class >> directions [
	"The allowable direction values"
	^ Dictionary newFrom: {
		0 -> #east .
		90 -> #north .
		180 -> #west .
		270 -> #south
	}
]

{ #category : #initialization }
Direction class >> east [ 
	^ self newFrom: #east
]

{ #category : #'instance creation' }
Direction class >> newFrom: aString [
	^ (self basicNew) direction: aString; yourself.
]

{ #category : #initialization }
Direction class >> north [ 
	^ self newFrom: #north
]

{ #category : #initialization }
Direction class >> south [ 
	^ self newFrom: #south
]

{ #category : #initialization }
Direction class >> west [ 
	^ self newFrom: #west
]

{ #category : #actions }
Direction >> advancement [
	"Returns a Position with the x and y set to the advancement increments"
	^ Position
		x: (direction degreeCos)
		y: (direction degreeSin)
]

{ #category : #printing }
Direction >> asString [
	^ (self class directions at: direction) asString
]

{ #category : #setter }
Direction >> direction: aString [
	direction := self class directions
		keyAtValue: aString asSymbol
		ifAbsent: [ self error: 'Invalid direction' ]
]

{ #category : #actions }
Direction >> turn: degrees [
	direction := (direction + degrees) \\ 360
]

{ #category : #actions }
Direction >> turnLeft [
	self turn: 90
]

{ #category : #actions }
Direction >> turnRight [
	self turn: -90
]
