Class {
	#name : #CollatzConjecture,
	#superclass : #Object,
	#category : #'Exercise@CollatzConjecture'
}

{ #category : #calculating }
CollatzConjecture >> stepsFrom: number [
	number isInteger ifFalse: [ self error: 'Only integers are allowed' ].
	number < 1 ifTrue: [ self error: 'Only positive numbers are allowed' ].
	^ self stepsFrom: number withSteps: 0
]

{ #category : #private }
CollatzConjecture >> stepsFrom: number withSteps: steps [
	^ number = 1 
		ifTrue: [ steps ]
		ifFalse: [ 
			self 
				stepsFrom: (number nextCollatzNumber)
				withSteps: steps + 1 ]
]
