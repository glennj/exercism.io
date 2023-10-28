Extension { #name : #Array }

{ #category : #'*Exercise@IsbnVerifier' }
Array >> isbnSum [
	"If it's an invalid ISBN array, return a number NOT divisible by 11"
	
	^ self isValidIsbnDigitArray 
		ifTrue: [ (self withIndexCollect: [ :d :i | d * (11 - i) ]) sum ]
		ifFalse: [ -1 ]
]

{ #category : #'*Exercise@IsbnVerifier' }
Array >> isValidIsbnDigitArray [

	^ self size = 10 and: [ (self allButLast includes: 10) not ]
]
