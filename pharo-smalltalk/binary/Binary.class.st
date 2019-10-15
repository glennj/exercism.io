Class {
	#name : #Binary,
	#superclass : #Object,
	#category : #'Exercise@Binary'
}

{ #category : #converting }
Binary >> decimalFromBinary: binaryString [
	| digits |
	digits := binaryString asArray collect: #digitValue.
	digits 
		detect: [ :d | (d < 0) | (d > 1) ]
		ifFound: [ ^ nil ].
	^ digits 
		inject: 0 
		into: [ :dec :d | 2 * dec + d ]
]
