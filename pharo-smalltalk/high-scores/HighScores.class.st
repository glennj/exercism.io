"
I can keep track of high scores.
"
Class {
	#name : #HighScores,
	#superclass : #Object,
	#instVars : [
		'scores'
	],
	#category : #'Exercise@HighScores'
}

{ #category : #adding }
HighScores >> addScores: listOfScores [
	scores addAll: listOfScores.
]

{ #category : #initialization }
HighScores >> initialize [
	scores := OrderedCollection new.
]

{ #category : #acccessing }
HighScores >> latestScore [
	^ scores last
]

{ #category : #acccessing }
HighScores >> personalBestScores [
	^ scores max
]

{ #category : #acccessing }
HighScores >> personalTopThreeScores [
	^ (scores sorted reversed takeFirst: 3) asArray
]

{ #category : #acccessing }
HighScores >> scores [
	^ scores asArray
]
