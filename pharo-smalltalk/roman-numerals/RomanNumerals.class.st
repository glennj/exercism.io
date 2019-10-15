Class {
	#name : #RomanNumerals,
	#superclass : #Object,
	#instVars : [
		'digitMap'
	],
	#category : #'Exercise@RomanNumerals'
}

{ #category : #initialization }
RomanNumerals >> initialize [
	digitMap := OrderedDictionary newFromPairs: { 
		1000 . 'M' . 900 . 'CM' . 500 . 'D' . 400 . 'CD' .
		 100 . 'C' .  90 . 'XC' .  50 . 'L' .  40 . 'XL' .
		  10 . 'X' .   9 . 'IX' .   5 . 'V' .   4 . 'IV' . 
		   1 . 'I' }
]

{ #category : #translating }
RomanNumerals >> romanNumber: anInteger [ 
	| num |
	num := anInteger.
		
	^ String streamContents: [ :roman |
		digitMap keysAndValuesDo: [ :n :r |
			[ num >= n ] 
				whileTrue: [ 
					num := num - n.
					roman nextPutAll: r ] ] ]
]
