Class {
	#name : #SecretHandshake,
	#superclass : #Object,
	#instVars : [
		'commands',
		'reverse'
	],
	#category : #'Exercise@SecretHandshake'
}

{ #category : #converting }
SecretHandshake >> commandsNumber: anInteger [ 
	| result |	
	result := commands withIndexSelect: [ :command :idx |
		(anInteger bitAt: idx) = 1 ].
	
	^(anInteger bitAt: reverse) = 1
		ifTrue: [ result reversed ]
		ifFalse: [ result ]
]

{ #category : #initialization }
SecretHandshake >> initialize [ 
	commands := #('wink' 'double blink' 'close your eyes' 'jump').
	reverse := 5.
]
