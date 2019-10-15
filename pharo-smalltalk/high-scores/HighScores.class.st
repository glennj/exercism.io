Class {
	#name : #HighScores,
	#superclass : #Object,
	#instVars : [
		'scores'
	],
	#category : #'Exercise@HighScores'
}

{ #category : #initialization }
HighScores >> initialize [
	scores := OrderedCollection new.
]

{ #category : #acccessing }
HighScores >> latestScore: aCollection [ 
	scores addAll: aCollection.
	^ scores last
]

{ #category : #acccessing }
HighScores >> personalBestScores: aCollection [ 
	scores addAll: aCollection.
	^ scores max
]

{ #category : #acccessing }
HighScores >> personalTopThreeScores: aCollection [ 
	scores addAll: aCollection.
	
	"take 1: `first:` will error if not enough elements"
	"
	| sorted |
	sorted := scores sorted reversed.
	^ ([ sorted first: 3 ] ifError: [ sorted ]) asArray
	"
	
	^ (scores sorted reversed 
		withIndexSelect: [ :elem :i | i <= 3 ])
		asArray
]

{ #category : #setting }
HighScores >> scores: aCollection [ 
	scores addAll: aCollection.
	^ scores copy asArray
]
