"
Determine if a number is an Armstrong number.

Calculate the sum of each digit raised to the number of digits.
If that sum equals the number, then it is an Armstrong number.
"
Class {
	#name : #ArmstrongNumbers,
	#superclass : #Object,
	#category : #'Exercise@ArmstrongNumbers'
}

{ #category : #'as yet unclassified' }
ArmstrongNumbers >> armstrongDigits: anInteger [
	"A collection of (each digit raised to the number of digits)"
   | len |

	len := anInteger decimalDigitLength.
	^ (1 to: len)
		collect: [ :i |
			(anInteger decimalDigitAt: i) raisedTo: len ]

]

{ #category : #'as yet unclassified' }
ArmstrongNumbers >> isArmstrongNumber: anInteger [ 
	^ ((self armstrongDigits: anInteger) sum) = anInteger.

]
