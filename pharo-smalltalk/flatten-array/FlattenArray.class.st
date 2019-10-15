Class {
	#name : #FlattenArray,
	#superclass : #Object,
	#category : #'Exercise@FlattenArray'
}

{ #category : #enumerating }
FlattenArray >> flattenArray: aCollection [ 
	^(Array streamContents: [ :flattened |
		aCollection do: [ :element |
			flattened nextPutAll: ( 
				(element isKindOf: Array)
					ifTrue: [ self flattenArray: element ]
					ifFalse: [ { element } ] ) ] ] )
		select: #isNotNil
]

{ #category : #enumerating }
FlattenArray >> flattenArray_iterative: aCollection [ 
	| result tmp looping |
	tmp := aCollection deepCopy.
	looping := true.
	
	[ looping ] whileTrue: [ 
		| countSubarrays |
		countSubarrays := 0.
		result := OrderedCollection new.
		tmp do: [ :elem |
			(elem isKindOf: Array) 
				ifTrue: [
					countSubarrays := countSubarrays + 1.
					result addAll: elem ]
				ifFalse: [ 
					elem ifNotNil: [ 
						result add: elem ] ] ].
		countSubarrays = 0 
			ifTrue: [ looping := false ]
			ifFalse: [ tmp := result deepCopy ] ].
	
	^ result asArray
]
