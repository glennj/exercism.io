Class {
	#name : #Allergies,
	#superclass : #Object,
	#category : #'Exercise@Allergies'
}

{ #category : #'as yet unclassified' }
Allergies class >> allergens [
	^ {
		'eggs'         ->   1 .
		'peanuts'      ->   2 .
		'shellfish'    ->   4 .
		'strawberries' ->   8 .
		'tomatoes'     ->  16 .
		'chocolate'    ->  32 .
		'pollen'       ->  64 .
		'cats'         -> 128
	} asOrderedDictionary
]

{ #category : #querying }
Allergies >> allergicToItem: item forScore: score [ 
	^ self 
		allergicToItemValue: (self class allergens at: item ifAbsent: 0)
		forScore: score
]

{ #category : #querying }
Allergies >> allergicToItemValue: value forScore: score [
	^ (score & value) isZero not
]

{ #category : #querying }
Allergies >> allergyListForScore: score [
	^(self class allergens 
		associationsSelect: [ :each |
			self allergicToItemValue: each value forScore: score ])
		keys

]
