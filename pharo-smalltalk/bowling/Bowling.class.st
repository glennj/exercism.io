Class {
	#name : #Bowling,
	#superclass : #Object,
	#instVars : [
		'frame',
		'score',
		'bonuses',
		'currentFrame'
	],
	#category : #'Exercise@Bowling'
}

{ #category : #private }
Bowling >> addScore: roll [
	score := score + roll.
	"add any current strike/spare bonus rolls"
	bonuses withIndexDo: [ :b :i |
		score := score + roll.
		bonuses at: i put: b - 1 ].
	bonuses removeAllSuchThat: [ :b | b = 0 ]
]

{ #category : #private }
Bowling >> handleFrame: roll [
	frame = 10 ifTrue: [ ^ self handleTenthFrame: roll ].

	roll = 10 
		ifTrue: [ 
			"recall that we've already checked for too many pins,
			 so if roll = 10 then it's really a strike
			 on the first ball of the frame."
			bonuses add: 2.
			frame := frame + 1 ]
		ifFalse: [ 
			currentFrame isEmpty 
				ifTrue: [ 
					"this is the first roll of frame"
					currentFrame add: roll ]
				ifFalse: [ 
					"second roll of frame"
					currentFrame first + roll = 10
						ifTrue: [ 
							"spare"
							bonuses add: 1 ].
					frame := frame + 1.
					self initCurrentFrame ] ]
]

{ #category : #private }
Bowling >> handleTenthFrame: roll [
	currentFrame add: roll.
	(currentFrame size = 3) | ((currentFrame size = 2) & (currentFrame sum < 10))
		ifTrue: [ frame := frame + 1 ]
]

{ #category : #private }
Bowling >> initCurrentFrame [
	currentFrame := OrderedCollection new.
]

{ #category : #initialization }
Bowling >> initialize [
	score := 0.
	frame := 1.
	bonuses := OrderedCollection new.
	self initCurrentFrame.
]

{ #category : #private }
Bowling >> isTooManyPins: roll [
	^ frame = 10 
		ifTrue: [ self isTooManyPinsForTenthFrame: roll ]
		ifFalse: [ 
			currentFrame isNotEmpty and: [ (currentFrame first + roll) > 10 ] ]
]

{ #category : #private }
Bowling >> isTooManyPinsForTenthFrame: roll [
	^ currentFrame isNotEmpty and: [
		| nonStrikes |
		nonStrikes := currentFrame select: [ :pins | pins < 10 ].
		nonStrikes isNotEmpty and: [ 
		((nonStrikes size = 2) and: [ nonStrikes sum = 10 ]) not and: [ 
		(nonStrikes first + roll > 10) ] ] ]
]

{ #category : #private }
Bowling >> roll: roll [
	self 
		validateRoll: roll;
		addScore: roll;
		handleFrame: roll 
]

{ #category : #accessing }
Bowling >> score [
	frame <= 10 ifTrue: [ self error: 'Score cannot be taken until the end of the game' ].
	^ score
]

{ #category : #calculating }
Bowling >> scoreAfterRolling: rolls [
	rolls do: [ :roll | self roll: roll ].
	^ self score
]

{ #category : #calculating }
Bowling >> scoreRolling: roll after: pastRolls [
	pastRolls do: [ :r | self roll: r ].
	self roll: roll.
	^ self score
]

{ #category : #private }
Bowling >> validateRoll: roll [
	frame > 10 ifTrue: [ self error: 'Cannot roll after game is over' ].
	roll < 0   ifTrue: [ self error: 'Negative roll is invalid' ].
	(roll > 10 or: [ self isTooManyPins: roll ])
		ifTrue: [ self error: 'Pin count exceeds pins on the lane' ].
]
