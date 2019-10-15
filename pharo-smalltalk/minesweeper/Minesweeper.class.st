Class {
	#name : #Minesweeper,
	#superclass : #Object,
	#category : #'Exercise@Minesweeper'
}

{ #category : #transforming }
Minesweeper >> annotateMinefield: aCollection [ 
	aCollection isEmpty ifTrue: [ ^ #() ].
	^ self toArray: (
		self process: (
			self array2dFrom: aCollection))
]

{ #category : #private }
Minesweeper >> array2dFrom: input [
	| arrayOfArrays |
	arrayOfArrays := input copy collect: #asArray.
	^ Array2D 
		rows: arrayOfArrays size 
		columns: (arrayOfArrays at: 1) size 
		tabulate: [ :x :y | (arrayOfArrays at: x) at: y ]
]

{ #category : #private }
Minesweeper >> incrementNeighboursAtX: x y: y in: minefield [
	(x - 1) to: (x + 1) do: [ :xx |
	(y - 1) to: (y + 1) do: [ :yy |
		((xx between: 1 and: minefield rowCount) and: [ 
		 (yy between: 1 and: minefield columnCount) and: [ 
		 ((minefield at: xx at: yy) ~= $*) ] ] )
			ifTrue: [ 
				| value |
				value := minefield at: xx at: yy.
				value = Character space ifTrue: [ value := 0 ].
				minefield at: xx at: yy put: value + 1 ] ] ]
]

{ #category : #private }
Minesweeper >> process: minefield [
	minefield indicesDo: [ :x :y | 
		(minefield at: x at: y) = $*
			ifTrue: [ self incrementNeighboursAtX: x y: y in: minefield] ].
	^ minefield
]

{ #category : #private }
Minesweeper >> toArray: minefield [
	^ (1 to: minefield rowCount)
		collect: [ :x | '' join: (minefield atRow: x) ]
]
