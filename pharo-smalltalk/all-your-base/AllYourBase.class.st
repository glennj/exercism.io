"
I can convert an array of numbers (representing the digits of a number in the ""input base"") to an array of numbers in the ""output base"".
"
Class {
	#name : 'AllYourBase',
	#superclass : 'Object',
	#category : 'Exercise@AllYourBase',
	#package : 'Exercise@AllYourBase'
}

{ #category : 'as yet unclassified' }
AllYourBase >> decimalValue: decimalValue toDigitsOfBase: outputBase [
	
	^ (decimalValue = 0)
		ifTrue: [ #(0) ]
		ifFalse: [ self decimalValue: decimalValue toDigitsOfBase: outputBase with: (OrderedCollection new) ]
]

{ #category : 'as yet unclassified' }
AllYourBase >> decimalValue: decimalValue toDigitsOfBase: outputBase with: digits [
	
	^ (decimalValue = 0)
		ifTrue: [ digits asArray ]
		ifFalse: [ 
			self decimalValue: (decimalValue // outputBase)
				toDigitsOfBase: outputBase 
				with: (digits addFirst: (decimalValue % outputBase); yourself) ]
]

{ #category : 'as yet unclassified' }
AllYourBase >> decimalValueOfDigits: digits inBase: inputBase [

	(digits allSatisfy: [ :digit | digit between: 0 and: inputBase - 1 ])
		ifFalse: [ self error: 'all digits must satisfy 0 <= d < input base' ].

	^ digits inject: 0 into: [ :dec :digit | dec * inputBase + digit ]
]

{ #category : 'as yet unclassified' }
AllYourBase >> rebaseInputBase: inputBase digits: digits outputBase: outputBase [

	(inputBase < 2) ifTrue: [ self error: 'input base must be >= 2' ].
	(outputBase < 2) ifTrue: [ self error: 'output base must be >= 2' ].

	^ self decimalValue: (self decimalValueOfDigits: digits inBase: inputBase) 
		toDigitsOfBase: outputBase
]
