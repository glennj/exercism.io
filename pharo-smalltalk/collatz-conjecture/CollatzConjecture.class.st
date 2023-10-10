"
I can determine the number of steps needed to reach 1 from a given positive integer, using the Collatz conjecture method.
"
Class {
	#name : #CollatzConjecture,
	#superclass : #Object,
	#category : #'Exercise@CollatzConjecture'
}

{ #category : #calculating }
CollatzConjecture >> stepsFrom: number [
	number isInteger ifFalse: [ self error: 'Only integers are allowed' ].
	number < 1 ifTrue: [ self error: 'Only positive numbers are allowed' ].

	^ number collatzSteps
	 
]
