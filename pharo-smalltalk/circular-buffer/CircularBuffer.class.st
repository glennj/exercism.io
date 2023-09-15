"
I represent a ""circular buffer"". I have a capacity. 

I can write an item to the buffer. If the buffer is full (the buffer size = the capacity) I will throw an error.

I can read an item from the buffer in the same order as they were written. If the buffer is empty I will throw an error.

I can overwrite an item. This is like a regular write except if the buffer is full, in which case I will overwrite the oldest item.
"
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
