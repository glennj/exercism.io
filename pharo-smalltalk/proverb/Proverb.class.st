Class {
	#name : #Proverb,
	#superclass : #Object,
	#category : #'Exercise@Proverb'
}

{ #category : #'as yet unclassified' }
Proverb >> reciteStrings: itemsCollection [ 
	| lines prev |
	lines := Array new: itemsCollection size.
	
	itemsCollection withIndexDo: [ :item :idx |
		idx = 1
			ifTrue: [ 
				lines at: lines size 
					put: ('And all for the want of a {1}.' format: {item}) ]
			ifFalse: [ 
				lines at: idx - 1 
					put: ('For want of a {1} the {2} was lost.' format: {prev . item}) ].
		prev := item ].
	
	^ lines
]
