Class {
	#name : #CircularBuffer,
	#superclass : #Object,
	#instVars : [
		'capacity',
		'buffer'
	],
	#category : #'Exercise@CircularBuffer'
}

{ #category : #accessing }
CircularBuffer >> capacity: aSize [
	
	capacity := aSize.
]

{ #category : #writing }
CircularBuffer >> clear [
    
	buffer := OrderedCollection new.
]

{ #category : #initialization }
CircularBuffer >> initialize [
	self clear
]

{ #category : #testing }
CircularBuffer >> isEmpty [

	^ buffer isEmpty
]

{ #category : #testing }
CircularBuffer >> isFull [

	^ buffer size = capacity
]

{ #category : #writing }
CircularBuffer >> overwrite: aValue [

	self isFull ifTrue: [ self read ].
	^ self write: aValue
]

{ #category : #accessing }
CircularBuffer >> read [
	self isEmpty ifTrue: [ ^ self error: 'no data'. ].
	^ buffer removeFirst
]

{ #category : #writing }
CircularBuffer >> write: aValue [
	self isFull ifTrue: [ ^ self error: 'buffer is full' ].
	buffer add: aValue.
]
