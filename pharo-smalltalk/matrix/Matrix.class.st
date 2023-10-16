"
I represent a simple rectangular matrix. My input is a newline-separated string. Each line in the string is a space-separated list of values. I can return a row or a column of the input.
"
Class {
	#name : #Matrix,
	#superclass : #Object,
	#category : #'Exercise@Matrix'
}

{ #category : #accessing }
Matrix >> columns: input [
	| rows |
	rows := self rows: input.

	"transpose the rows"	
	^ (1 to: rows first size) collect: [ :i |
		rows collect: [ :row | row at: i ] ]
]

{ #category : #accessing }
Matrix >> input: input atColumn: n [
	^ ((self columns: input) at: n) asArray
]

{ #category : #accessing }
Matrix >> input: input atRow: n [
	^ ((self rows: input) at: n) asArray
]

{ #category : #accessing }
Matrix >> rows: input [
	^ (input findTokens: Character cr)
		collect: [ :row | 
			(row findTokens: Character separators)
				collect: #asInteger ]
]
