Class {
	#name : #Luhn,
	#superclass : #Object,
	#category : #'Exercise@Luhn'
}

{ #category : #private }
Luhn >> hasInvalidCharacters: aString [
	"Only digits and spaces allowed."
	^ aString asArray 
		detect:  [ :c | ((c isDigit) | (c = Character space)) not ]
		ifFound: [ ^ true ]
		ifNone:  [ ^ false ]
]

{ #category : #lookup }
Luhn >> luhnDouble: n [
	"Given digit n, double it.
	 If the doubled value m is > 9, sum the digits of m."
	"Can also be written as: (2 * n) digitSum"

	^ #( 0 2 4 6 8 1 3 5 7 9 ) at: (n + 1)
]

{ #category : #validating }
Luhn >> validValue: value [
	| digits |
	
	(self hasInvalidCharacters: value) ifTrue: [ ^ false ].
	
	digits := value asArray select: #isDigit thenCollect: #digitValue.
	
	digits size <= 1 ifTrue: [ ^ false ].
	
	^ (digits
		reversed 
		withIndexCollect: [ :d :i |
			i odd 
				ifTrue: [ d ] 
				ifFalse: [ self luhnDouble: d ] ] )
		sum
		isDivisibleBy: 10
]
