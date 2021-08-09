Class {
	#name : #Diamond,
	#superclass : #Object,
	#category : #'Exercise@Diamond'
}

{ #category : #generating }
Diamond >> rowsLetter: letterString [
	| alphabet letter size spaces rows |
	letter := letterString first.
	alphabet := Character alphabet asUppercase.

	(alphabet includes: letter)
		ifFalse: [ ^ self error: 'invalid letter ', letterString ].

	size := alphabet indexOf: letter.
	rows := Array new: 2 * size - 1.
	spaces := ' ' repeat: size.

	($A to: letter) withIndexDo: [ :char :i |
		| halfRow row |
		"the right-hand half"
		halfRow := spaces copy.
		halfRow at: i put: char.
		"the whole row"
		row :=  halfRow allButFirst reversed , halfRow.
		"add the row to the array into both hemispheres"
		rows at: i put: row.
		rows at: (2 * size - i) put: row ].

	^ rows
]
