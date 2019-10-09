Class {
	#name : #MatrixReader,
	#superclass : #Object,
	#category : #'Exercise@Matrix'
}

{ #category : #accessing }
MatrixReader >> columns: input [
	| rows |
	rows := self rows: input.

	"transpose the rows"	
	^ (1 to: rows first size) collect: [ :i |
		rows collect: [ :row | row at: i ] ]
]

{ #category : #'as yet unclassified' }
MatrixReader >> input: input atColumn: n [
	^ ((self columns: input) at: n) asArray
]

{ #category : #'as yet unclassified' }
MatrixReader >> input: input atRow: n [
	^ ((self rows: input) at: n) asArray
]

{ #category : #'instance creation' }
MatrixReader >> rows: input [
	^ (input findTokens: Character cr)
		collect: [ :row | 
			(row findTokens: Character separators)
				collect: [ :intString | intString asInteger ] ]
]
