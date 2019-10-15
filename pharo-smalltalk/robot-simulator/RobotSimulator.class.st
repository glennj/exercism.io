Class {
	#name : #RobotSimulator,
	#superclass : #Object,
	#instVars : [
		'direction',
		'position'
	],
	#category : #'Exercise@RobotSimulator'
}

{ #category : #constants }
RobotSimulator class >> advances [
	"Maps a direction to a (xDelta yDelta) pair"
	^ Dictionary newFromPairs: { 
		'east'  . { 'x' ->  1 . 'y' ->  0 } asDictionary .
		'north' . { 'x' ->  0 . 'y' ->  1 } asDictionary .
		'west'  . { 'x' -> -1 . 'y' ->  0 } asDictionary .
		'south' . { 'x' ->  0 . 'y' -> -1 } asDictionary }
]

{ #category : #constants }
RobotSimulator class >> directions [
	^ #('north' 'east' 'south' 'west')
]

{ #category : #private }
RobotSimulator >> actionA [
	"Advance the robot 1 step in the current direction."
	| delta |
	delta := self class advances at: direction.
	position addX: (delta at: 'x') y: (delta at: 'y')
]

{ #category : #private }
RobotSimulator >> actionL [
	"Turn Left (counterclockwise)."
	self turn: -1
]

{ #category : #private }
RobotSimulator >> actionR [
	"Turn Right (clockwise)."
	self turn: 1
]

{ #category : #converting }
RobotSimulator >> asDictionary [
	^ (Dictionary new)
		add: ('direction' -> direction);
		add: ('position' -> position asDictionary);
		yourself
]

{ #category : #initialization }
RobotSimulator >> initialize [ 
	super initialize.
	direction := 'north'.
	position := Position x: 0 y: 0.
]

{ #category : #controlling }
RobotSimulator >> sendInstructions: aString [ 
	aString asUppercase asArray do: [ :instruction |
		('RAL' includes: instruction)
			ifFalse: [ self error: 'Unknown instruction ', instruction ].
		self perform: ('action' , instruction asString) asSymbol ].
	^ self asDictionary
]

{ #category : #private }
RobotSimulator >> setDirection: directionName [
	(self class directions includes: directionName)
		ifTrue: [ direction := directionName ]
		ifFalse: [ self error: 'Invalid direction ', directionName ]
]

{ #category : #private }
RobotSimulator >> setPosition: aDictionary [
	(aDictionary includesKey: 'x') & (aDictionary includesKey: 'y')
		ifTrue: [ position := Position x: (aDictionary at: 'x') y: (aDictionary at: 'y') ]
		ifFalse: [ self error: 'Invalid position dictionary' ]
]

{ #category : #controlling }
RobotSimulator >> startFacing: aString atPosition: aCollection [ 
	self setDirection: aString.
	self setPosition: aCollection.
	^ self asDictionary
]

{ #category : #private }
RobotSimulator >> turn: increment [
	| directions nextIndex |
	directions := self class directions.
	nextIndex := (directions indexOf: direction) + increment.
	nextIndex = (directions size + 1) ifTrue: [ nextIndex := 1 ].
	nextIndex = 0 ifTrue: [ nextIndex := directions size ].
	self setDirection: (directions at: nextIndex)
]
