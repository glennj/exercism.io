Class {
	#name : #Forth,
	#superclass : #Object,
	#instVars : [
		'macros',
		'stack'
	],
	#category : #'Exercise@Forth'
}

{ #category : #evaluating }
Forth >> binaryArithmeticOperation: operator [
	|a b|
	self requireMinimumStackSize: 2.
	b := stack pop.
	a := stack pop.
	
	(operator = '//' and: (b = 0))
		ifTrue: [ self error: 'divide by zero' ].
	
	stack push: (a perform: operator asSymbol with: b) 
]

{ #category : #evaluating }
Forth >> defineMacro: tokens [
   | name replacement |

	name := tokens removeFirst.
	name isAllDigits ifTrue: [ self error: 'illegal operation' ].
	
	(tokens last = ';')
		ifTrue: [ tokens removeLast ]
		ifFalse: [ self error: 'missing semicolon for macro' ].
	
	replacement := OrderedCollection new.
	tokens do: [ :token | 
		replacement addAll: (macros at: token ifAbsent: {token}) ].
	
		" note to self: braces required in the `ifAbsent:` argument
		  to prevent string from being expanded into a sequence of characters
		  in the replacement. Braces add a layer of 'collection-ness'
		  around the string that `addAll:` strips away. "
	
	macros at: name put: replacement
]

{ #category : #evaluating }
Forth >> drop [
	self requireMinimumStackSize: 1.
	stack pop
]

{ #category : #evaluating }
Forth >> dup [
	self requireMinimumStackSize: 1.
	stack push: stack top
]

{ #category : #evaluating }
Forth >> evaluateInstruction: instruction [
	| tokens |
	tokens := instruction asLowercase findTokens: Character separators.
	
	tokens first = ':'
		ifTrue: [ self defineMacro: tokens allButFirst ]
		ifFalse: [ 
			[ tokens isEmpty ]
				whileFalse: [
					tokens := self processInstructionTokens: tokens ] ]

]

{ #category : #evaluating }
Forth >> evaluateInstructions: instructionList [
	instructionList do: [ :instruction |
		self evaluateInstruction: instruction ].
	^ stack reversed 
]

{ #category : #initialization }
Forth >> initialize [
	stack := Stack new.
	macros := Dictionary new.
]

{ #category : #evaluating }
Forth >> over [
	| a b |
	self requireMinimumStackSize: 2.
	b := stack pop.
	a := stack top.
	stack push: b.
	stack push: a
]

{ #category : #evaluating }
Forth >> processInstructionTokens: tokens [
	| token |
	token := tokens removeFirst.
			
	"1. if the first token is a macro, inject the definition into the tokens list"
	(macros includesKey: token)
		ifTrue: [ 
			tokens addAllFirst: (macros at: token).
			^ tokens ].
				
	"2. if the token is a number, add it to the stack"
	token isAllDigits
		ifTrue: [ 
			stack push: token asInteger.
			^ tokens ].

	"3. if the token is a binary operator, process it."
	(#('+' '-' '*' '/') includes: token)
		ifTrue: [ 
			token = '/' ifTrue: [ token := '//' ].
			self binaryArithmeticOperation: token.
			^ tokens ].

	"4  process stack commands"				
	(#('dup' 'drop' 'swap' 'over') includes: token)
		ifTrue: [ 
			self perform: token asSymbol.
			^ tokens ].

	"5. otherwise don't know what this is"
	self error: 'undefined operation'.
]

{ #category : #evaluating }
Forth >> requireMinimumStackSize: n [
	(n > 0 and: (stack isEmpty))
		ifTrue: [ self error: 'empty stack' ].
	(n > 1 and: (stack size = 1))
		ifTrue: [ self error: 'only one value on the stack' ].
]

{ #category : #evaluating }
Forth >> swap [
	| a b |
	self requireMinimumStackSize: 2.
	b := stack pop.
	a := stack pop.
	stack push: b.
	stack push: a
]
