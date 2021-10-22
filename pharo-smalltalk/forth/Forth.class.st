"
I implement the Forth programming language.
I can evaluate a list of instructions and return the values on the stack.
"
Class {
	#name : #Forth,
	#superclass : #Object,
	#instVars : [
		'stack'
	],
	#category : #'Exercise@Forth'
}

{ #category : #evaluating }
Forth >> evaluateInstruction: instruction [
	| tokens |
	tokens := instruction asLowercase findTokens: Character separators.
	
	tokens first = ':'
		ifTrue: [ 
			" assume the last token is ';' "
			stack defineOperation: tokens allButFirst allButLast ]
		ifFalse: [
			tokens do: [ :token |
				stack execute: token ] ]

]

{ #category : #evaluating }
Forth >> evaluateInstructions: instructionList [
	instructionList do: [ :instruction |
		self evaluateInstruction: instruction ].
	^ stack asArray 
]

{ #category : #initialization }
Forth >> initialize [
	stack := ForthStack new
]
