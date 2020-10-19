"
I am a robot that can be given some instructions.
"
Class {
	#name : #RobotSimulator,
	#superclass : #Object,
	#instVars : [
		'direction',
		'position'
	],
	#category : #'Exercise@RobotSimulator'
}

{ #category : #actions }
RobotSimulator >> advance [
	"Advance the robot 1 step in the current direction."
	position advance: direction
]

{ #category : #converting }
RobotSimulator >> asDictionary [
	^ (Dictionary new)
		add: ('direction' -> direction asString);
		add: ('position' -> position asDictionary);
		yourself
]

{ #category : #initialization }
RobotSimulator >> initialize [ 
	super initialize.
	direction := Direction north.
	position := Position zero.
]

{ #category : #controlling }
RobotSimulator >> sendInstructions: aString [ 
	| dispatch action |
	
	dispatch := Dictionary newFrom: { 
		$A -> #advance .
		$R -> #turnRight  .
		$L -> #turnLeft .
	}.
	
	aString asUppercase asArray do: [ :instruction |
		action := dispatch
			at: instruction
			ifAbsent: [ self error: 'Unknown instruction' ].
		self perform: action
	].
	
	^ self asDictionary
]

{ #category : #setter }
RobotSimulator >> setDirection: directionName [
	direction := Direction newFrom: directionName

]

{ #category : #setter }
RobotSimulator >> setPosition: aDictionary [
	(aDictionary includesKey: 'x') & (aDictionary includesKey: 'y')
		ifTrue: [ position := Position x: (aDictionary at: 'x') y: (aDictionary at: 'y') ]
		ifFalse: [ self error: 'Invalid position dictionary' ]
]

{ #category : #controlling }
RobotSimulator >> startFacing: aString atPosition: aCollection [ 
	^ self
		setDirection: aString;
		setPosition: aCollection;
		asDictionary
]

{ #category : #actions }
RobotSimulator >> turnLeft [
	"Turn Left (counterclockwise)."
	direction turnLeft

]

{ #category : #actions }
RobotSimulator >> turnRight [
	"Turn Right (clockwise)."
	direction turnRight
]
