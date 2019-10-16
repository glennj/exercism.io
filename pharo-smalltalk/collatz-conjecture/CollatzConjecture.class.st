Class {
	#name : #CollatzConjecture,
	#superclass : #Object,
	#category : #'Exercise@CollatzConjecture'
}

{ #category : #calculating }
CollatzConjecture >> stepsFrom: number [
	number < 1
		ifTrue: [ self error: 'Only positive numbers are allowed' ].
	^ self stepsFrom: number withSteps: 0
]

{ #category : #private }
CollatzConjecture >> stepsFrom: number withSteps: steps [
	^ number = 1 
		ifTrue: [ steps ]
		ifFalse: [ 
			self 
				stepsFrom: 
					((number isDivisibleBy: 2)
						ifTrue: [ number / 2]
						ifFalse: [ 3 * number + 1 ])
				withSteps: steps + 1 ]
]
