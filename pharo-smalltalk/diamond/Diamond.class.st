Class {
	#name : #Diamond,
	#superclass : #Object,
	#category : #'Exercise@Diamond'
}

{ #category : #'as yet unclassified' }
Diamond >> rowsLetter: letterString [
	| alphabet letter size spaces rows |
	letter := letterString first.
	alphabet := Character alphabet asUppercase.

	(alphabet includes: letter)
		ifFalse: [ self error: 'invalid letter ', letterString ].

	size := alphabet indexOf: letter.
	rows := Array new: 2 * size - 1.
	spaces := ' ' repeat: size.

	"build the top half"
	($A to: letter) withIndexDo: [ :char :i |
		| halfRow |
		halfRow := spaces copy.
		halfRow at: i put: char.
		rows at: i put: halfRow allButFirst reversed , halfRow ].

	"and mirror the bottom half"
	(rows first: size - 1) 
		reversed 
		withIndexDo: [ :row :i | 
			rows at: size + i put: row ].

	^ rows
]
