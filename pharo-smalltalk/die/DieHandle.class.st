Class {
	#name : #DieHandle,
	#superclass : #Object,
	#instVars : [
		'dice'
	],
	#category : #'Exercise@Die'
}

{ #category : #'instance creation' }
DieHandle class >> withNumDice: n ofFaces: f [
	^ self new addDice: 
		((1 to: n) collect: [ :i | Die withFaces: f ])
]

{ #category : #combining }
DieHandle >> + other [
	^self class new
		addDice: self dice;
		addDice: other dice;
		yourself.
]

{ #category : #'adding elements' }
DieHandle >> addDice: someDice [ 
	someDice do: [ :die | self addDie: die ]
]

{ #category : #'adding elements' }
DieHandle >> addDie: aDie [ 
	dice add: aDie
]

{ #category : #accessing }
DieHandle >> dice [
	^ dice
]

{ #category : #accessing }
DieHandle >> diceNumber [
	^ dice size
]

{ #category : #initialization }
DieHandle >> initialize [
	dice := OrderedCollection new.
]

{ #category : #action }
DieHandle >> roll [
	^ (dice collect: #roll) sum 
]
