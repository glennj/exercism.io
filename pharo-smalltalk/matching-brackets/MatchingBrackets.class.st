Class {
	#name : #MatchingBrackets,
	#superclass : #Object,
	#category : #'Exercise@MatchingBrackets'
}

{ #category : #testing }
MatchingBrackets >> isPaired: aString [ 
	| brackets stack |
	stack := Stack new.

	"map the _closing_ bracket to the _opening_ one"
	brackets := Dictionary newFromPairs: { 
		$} . ${ .
		$) . $( .
		$] . $[ }.
	
	aString asArray do: [ :char |
		"open brackets are added to the stack"
		(brackets includes: char)
			ifTrue: [ stack push: char ].
		
		"close brackets need a closer look"
		(brackets includesKey: char)
			ifTrue: [ 
				stack isEmpty ifTrue: [ ^ false ]. 
				(brackets at: char) = (stack pop)
					ifFalse: [ ^ false ] ] ].
		
	^ stack isEmpty
]
