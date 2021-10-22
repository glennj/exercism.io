"
I implement the stack for the Forth language.
I can add, subtract, etc.
"
Class {
	#name : #ForthStack,
	#superclass : #Object,
	#instVars : [
		'stack',
		'operations'
	],
	#category : #'Exercise@Forth'
}

{ #category : #adding }
ForthStack >> add [
	|a b|
	self requireStackSize: 2.
	a := stack pop.
	b := stack pop.
	stack push: a + b
]

{ #category : #converting }
ForthStack >> asArray [
	^ stack asArray reversed
]

{ #category : #'as yet unclassified' }
ForthStack >> defineOperation: tokens [
	|name|
	name := tokens removeFirst.
	name isAllDigits 
		ifTrue: [ self error: 'illegal operation: cannot redefine numbers' ].
		
	operations at: name put: (
		tokens flatCollect: [ :token | operations at: token ifAbsent: {token} ]
	)
]

{ #category : #adding }
ForthStack >> div [
	|a b|
	self requireStackSize: 2.
	a := stack pop.
	a isZero ifTrue: [ self error: 'cannot divide by zero' ].
	b := stack pop.
	stack push: b // a
]

{ #category : #adding }
ForthStack >> drop [
	self requireStackSize: 1.
	stack pop
]

{ #category : #adding }
ForthStack >> dup [
	self requireStackSize: 1.
	stack push: stack top
]

{ #category : #'as yet unclassified' }
ForthStack >> execute: token [

	token isAllDigits
		ifTrue: [ self push: token asInteger ]
		ifFalse: [ 
			operations at: token
				ifPresent: [ :operation |
					operation do: [ :op |
						op isSymbol 
							ifTrue: [ self perform: op ]
							ifFalse: [ self execute: op ] ] ]
				ifAbsent: [ 
					self error: 'undefined operation' ] ]
]

{ #category : #initialization }
ForthStack >> initialize [
	stack := Stack new.
	
	" 'Primitive' instructions are added as arrays of symbols.
	  Subsequent macros will be collections of strings.
	"
	operations := Dictionary newFrom: { 
		'+' -> #( #add ). 
		'-' -> #( #sub ).
		'*' -> #( #mul ).
		'/' -> #( #div ).
		'drop' -> #( #drop ).
		'dup'  -> #( #dup ).
		'over' -> #( #over ).
		'swap' -> #( #swap )
	}.
]

{ #category : #adding }
ForthStack >> mul [
	|a b|
	self requireStackSize: 2.
	a := stack pop.
	b := stack pop.
	stack push: a * b
]

{ #category : #adding }
ForthStack >> over [
	|a b|
	self requireStackSize: 2.
	a := stack pop.
	b := stack top.
	stack push: a; push: b
]

{ #category : #adding }
ForthStack >> push: element [
	stack push: element
]

{ #category : #'as yet unclassified' }
ForthStack >> requireStackSize: num [
	(num > 0 and: [ stack isEmpty ]) ifTrue: [ self error: 'cannot perform operation on empty stack' ].
	(num > 1 and: [ stack size == 1 ]) ifTrue: [ self error: 'cannot perform operation with only one value on the stack ' ]
]

{ #category : #adding }
ForthStack >> sub [
	|a b|
	self requireStackSize: 2.
	a := stack pop.
	b := stack pop.
	stack push: b - a
]

{ #category : #adding }
ForthStack >> swap [
	|a b|
	self requireStackSize: 2.
	a := stack pop.
	b := stack pop.
	stack push: a; push: b
]
