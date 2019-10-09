Class {
	#name : #GradeSchool,
	#superclass : #Object,
	#instVars : [
		'classes'
	],
	#category : #'Exercise@GradeSchool'
}

{ #category : #adding }
GradeSchool >> addStudents: listOfStudents [
	"each student in the list is a two-element collection: { name . grade }"
	
	listOfStudents do: [ :student |
		classes
			at: student last
			put: ((self class: student last) add: student first; yourself) ]
]

{ #category : #'as yet unclassified' }
GradeSchool >> class: grade [
	| class |
	class := classes at: grade ifAbsent: [SortedCollection new].
	^ class copyFrom: 1 to: class size

]

{ #category : #'as yet unclassified' }
GradeSchool >> desiredGrade: grade [
	^ (self class: grade) asArray
]

{ #category : #initialization }
GradeSchool >> initialize [
	classes := Dictionary new.
]

{ #category : #'as yet unclassified' }
GradeSchool >> roster [
	| roster |

	roster := OrderedCollection new.
	classes keys sorted do: [ :grade | roster addAll: (self class: grade) ].
	^ roster asArray
]
