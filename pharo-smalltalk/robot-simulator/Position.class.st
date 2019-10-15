Class {
	#name : #Position,
	#superclass : #Object,
	#instVars : [
		'x',
		'y'
	],
	#category : #'Exercise@RobotSimulator'
}

{ #category : #'instance creation' }
Position class >> x: xValue y: yValue [
	^ self new setX: xValue y: yValue
]

{ #category : #moves }
Position >> addX: xDelta y: yDelta [
	self setX: x + xDelta y: y + yDelta
]

{ #category : #converting }
Position >> asDictionary [
	^ Dictionary newFromPairs: { 'x' . x . 'y' . y }
]

{ #category : #initialization }
Position >> initialize [
	super initialize.
	x := 0.
	y := 0.
]

{ #category : #private }
Position >> setX: xValue y: yValue [
	x := xValue.
	y := yValue.
]

{ #category : #accessing }
Position >> x [
	^x
]

{ #category : #accessing }
Position >> y [
	^y
]
