"
I am a Simple Cipher. I can encode and decode messages. You can give me an enciphering key or I can make a random one.
"
Class {
	#name : #SimpleCipher,
	#superclass : #Object,
	#instVars : [
		'key'
	],
	#category : #'Exercise@SimpleCipher'
}

{ #category : #accessing }
SimpleCipher class >> randomKey [

	^ self randomKey: 100
]

{ #category : #enciphering }
SimpleCipher class >> randomKey: size [
	| rand alpha n |
	
	alpha := Character alphabet.
	n := alpha size.
	rand := Random new.
	
	^ String new: size streamContents: [ :stream |
		size timesRepeat: [ 
			stream nextPut: (alpha at: (rand nextInteger: n)) ] ]
]

{ #category : #enciphering }
SimpleCipher >> decode: cipherText [
	^ self encipher: cipherText direction: -1

]

{ #category : #enciphering }
SimpleCipher >> encipher: text direction: dir [
	self ensureKeyLength: text size.
	
	^ String new: (text size) streamContents: [ :stream |
		1 to: (text size) do: [ :i | 
			stream nextPut: (self encipherCharacter: (text at: i) with: (self key at: i) direction: dir) ] ]

]

{ #category : #enciphering }
SimpleCipher >> encipherCharacter: c1 with: c2 direction: d [
	| alpha i j k |
	
	alpha := Character alphabet.
	i := alpha indexOf: c1 ifAbsent: 1.
	j := alpha indexOf: c2 ifAbsent: 1.
	k := ((i + (d * (j - 1))) \\ (alpha size)).
	k = 0 ifTrue: [ k := alpha size ].
	^ alpha at: k
]

{ #category : #enciphering }
SimpleCipher >> encode: plainText [
	^ self encipher: plainText direction: 1

]

{ #category : #enciphering }
SimpleCipher >> ensureKeyLength: size [
	[ self key size < size ] whileTrue: [ key := key , key ]
]

{ #category : #accessing }
SimpleCipher >> key [
	key ifNil: [ key := self class randomKey ].
	^ key
]

{ #category : #accessing }
SimpleCipher >> key: aString [
	key := aString.
	^ self
]
