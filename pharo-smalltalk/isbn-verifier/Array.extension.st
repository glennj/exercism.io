Extension { #name : #Array }

{ #category : #'*Exercise@IsbnVerifier' }
Array >> isbnSum [

	^ (self withIndexCollect: [ :d :i | d * (11 - i) ]) sum
]

{ #category : #'*Exercise@IsbnVerifier' }
Array >> isValidIsbnDigitArray [

	^ self size = 10 and: [ 
		(self allSatisfy: #isInteger) and: [
		(self allSatisfy: [:i | i between: 0 and: 10 ]) and: [ 
		(self allButLast includes: 10) not ]]]
]
