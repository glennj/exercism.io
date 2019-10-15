"
# Tournament

Tally the results of a small football competition.

"
Class {
	#name : #Tournament,
	#superclass : #Object,
	#instVars : [
		'teams'
	],
	#category : #'Exercise@Tournament'
}

{ #category : #parsing }
Tournament >> parseResults: resultsCollection [
	teams := OrderedCollection new.

	resultsCollection do: [ :resultString |
		| items |
		items := resultString splitOn: $;.
		self 
			result: (items at: 3) 
			withHome: (items at: 1) 
			andAway: (items at: 2) ]
]

{ #category : #private }
Tournament >> report [
	^ OrderedCollection new
		add: Team header;
		addAll: ((teams sorted: [ :t1 :t2 | t1 > t2 ]) collect: #asString);
		asArray
]

{ #category : #private }
Tournament >> result: result withHome: homeName andAway: awayName [
	| home away |
	home := self team: homeName.
	away := self team: awayName.
			
	result = 'win'  ifTrue: [ home win.  away loss ] ifFalse: [ 
	result = 'loss' ifTrue: [ home loss. away win  ] ifFalse: [ 
	result = 'draw' ifTrue: [ home draw. away draw ] 
		ifFalse: [ self error: 'Invalid result' ] ] ]
]

{ #category : #reporting }
Tournament >> tallyRows: resultsCollection [
	^ self 
		parseResults: resultsCollection;
		report
]

{ #category : #private }
Tournament >> team: name [
	| team |
	team := teams select: [ :t | t name = name ].
	team isEmpty ifFalse: [ ^ team first ].
	team := Team new name: name.
	teams add: team.
	^ team
]
