"
# ISBN Verifier

The [ISBN-10 verification process](https://en.wikipedia.org/wiki/International_Standard_Book_Number) is used to validate book identification
numbers. These normally contain dashes and look like: `3-598-21508-8`

Refer to the wikipedia entry for more detail.

Example:

```
| verifier |
verifier := IsbnVerifier new.
verifier isValidIsbn: '0-306-40615-2'. >>> true
verifier isValidIsbn: '0-306-40615-3'. >>> false
```

"
Class {
	#name : #IsbnVerifier,
	#superclass : #Object,
	#category : #'Exercise@IsbnVerifier'
}

{ #category : #validating }
IsbnVerifier >> isValidIsbn: aString [ 

	^ (self isbnDigits: aString) isbnSum isDivisibleBy: 11
]

{ #category : #transforming }
IsbnVerifier >> isbnDigits: aString [

	^ aString asArray select: #isIsbnCharacter thenCollect: #isbnCharacterValue
]
