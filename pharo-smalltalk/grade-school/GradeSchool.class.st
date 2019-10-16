Class {
	#name : #GradeSchool,
	#superclass : #Object,
	#instVars : [
		'grades'
	],
	#category : #'Exercise@GradeSchool'
}

{ #category : #adding }
GradeSchool >> addStudents: listOfStudents [
	"each student in the list is a two-element collection: { name . grade }"
	
	listOfStudents do: [ :student |
		grades
			at: student last
			put: ((self grade: student last) add: student first; yourself) ]
]

{ #category : #accessing }
GradeSchool >> desiredGrade: grade [
	^ (self grade: grade) asArray
]

{ #category : #private }
GradeSchool >> grade: grade [
	^ (grades at: grade ifAbsent: [ SortedCollection new ]) copy
]

{ #category : #initialization }
GradeSchool >> initialize [
	grades := Dictionary new.
]

{ #category : #accessing }
GradeSchool >> roster [
	| roster |

	roster := OrderedCollection new.
	grades keys sorted do: [ :grade | roster addAll: (self grade: grade) ].
	^ roster asArray
]
