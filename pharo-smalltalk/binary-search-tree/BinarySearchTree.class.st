"
I am a binary search tree. See [https://en.wikipedia.org/wiki/Binary_search_tree](https://en.wikipedia.org/wiki/Binary_search_tree)
"
Class {
	#name : #BinarySearchTree,
	#superclass : #Object,
	#instVars : [
		'data',
		'left',
		'right'
	],
	#category : #'Exercise@BinarySearchTree'
}

{ #category : #adding }
BinarySearchTree >> add: aValue [
	data ifNil: [ data := aValue. ^ aValue ].
	
	aValue <= data 
		ifTrue: [
			left ifNil: [ left := self class new ].
			^ left add: aValue
		]
		ifFalse: [ 
			right ifNil: [ right := self class new ].
			^ right add: aValue
 		]
]

{ #category : #accessing }
BinarySearchTree >> data [
	^ data
]

{ #category : #accessing }
BinarySearchTree >> left [
	^ left
]

{ #category : #accessing }
BinarySearchTree >> left: aTree [
	left := aTree
]

{ #category : #accessing }
BinarySearchTree >> right [
	^ right
]

{ #category : #accessing }
BinarySearchTree >> right: aTree [
	right := aTree
]

{ #category : #accessing }
BinarySearchTree >> sortedData [

	|values|
	values := OrderedCollection new.
	
	left ifNotNil: [ values addAll: left sortedData ].
	data ifNotNil: [ values add: data ].
	right ifNotNil: [ values addAll: right sortedData ].

	^ values asArray 
]

{ #category : #adding }
BinarySearchTree >> treeData: aCollection [

	aCollection do: [ :aValue | self add: aValue ].
]
