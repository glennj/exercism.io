Class {
	#name : #Binary,
	#superclass : #Object,
	#category : #'Exercise@Binary'
}

{ #category : #converting }
Binary >> convert: aString toDecimalFromBase: fromBase [
	"add this generic method to pave the way for future All Your Base exercise"
	| fromDigits |

	"`digitValue` properly handles up to base 36"
	fromDigits := aString asArray collect: #digitValue.
	
	"check for invalid digits"
	(fromDigits 
		allSatisfy: [ :d | d between: 0 and: fromBase - 1 ])
		ifFalse: [ ^ nil ].
		
	"calculate decimal value"
	^ fromDigits 
		inject: 0 
		into: [ :dec :d | dec * fromBase + d ]
]

{ #category : #converting }
Binary >> decimalFromBinary: binaryString [
	^ self convert: binaryString toDecimalFromBase: 2
]
