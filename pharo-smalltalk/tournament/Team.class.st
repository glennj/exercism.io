"
Object to represent a team playing in the tournament.
"
Class {
	#name : #Team,
	#superclass : #Object,
	#instVars : [
		'name',
		'wins',
		'losses',
		'draws'
	],
	#category : #'Exercise@Tournament'
}

{ #category : #reporting }
Team class >> formatRow: data [
	" 'Team                           | MP |  W |  D |  L |  P' "
	^ '{1} | {2} | {3} | {4} | {5} | {6}' format: { 
		((data at: 1) padRightTo: 30) .
		((data at: 2) padLeftTo: 2) .
		((data at: 3) padLeftTo: 2) .
		((data at: 4) padLeftTo: 2) .
		((data at: 5) padLeftTo: 2) .
		((data at: 6) padLeftTo: 2) }
]

{ #category : #reporting }
Team class >> header [
    ^ self formatRow: #('Team' 'MP' 'W' 'D' 'L' 'P')
]

{ #category : #comparing }
Team >> > otherTeam [
	self points > otherTeam points ifTrue: [ ^ true ] ifFalse: [ 
	self points < otherTeam points ifTrue: [ ^ false ] 
		ifFalse: [ ^ (self name) < (otherTeam name) ] ]	
]

{ #category : #results }
Team >> draw [
	draws := draws + 1
]

{ #category : #accessing }
Team >> draws [
	^ draws
]

{ #category : #initialization }
Team >> initialize [
	name := ''.
	wins := 0.
	losses := 0.
	draws := 0.
]

{ #category : #results }
Team >> loss [
	losses := losses + 1
]

{ #category : #accessing }
Team >> losses [
	^ losses
]

{ #category : #accessing }
Team >> matchesPlayed [
	^ wins + losses + draws
]

{ #category : #accessing }
Team >> name [
	^ name
]

{ #category : #accessing }
Team >> name: aString [
	name := aString
]

{ #category : #accessing }
Team >> points [
	^ 3 * wins + draws
]

{ #category : #printing }
Team >> printOn: aStream [
	aStream nextPutAll: 
		(self class formatRow: { 
			self name .
			self matchesPlayed asString .
			self wins asString .
			self draws asString .
			self losses asString .
			self points asString } )
]

{ #category : #results }
Team >> win [
	wins := wins + 1
]

{ #category : #accessing }
Team >> wins [
	^ wins
]
