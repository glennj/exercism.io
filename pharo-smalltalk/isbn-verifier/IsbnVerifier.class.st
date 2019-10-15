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
	| digits |
	digits := self isbnDigits: aString.
	
	^ digits isNotEmpty and: [ 
		((digits withIndexCollect: [ :d :i | d * (11 - i) ]) sum) 
			isDivisibleBy: 11 ]
]

{ #category : #transforming }
IsbnVerifier >> isbnDigits: aString [
	"if the ISBN string is well-formed, returns an collection of digit values.
	 otherwise, returns an empty collection.
	 examples:
	   self isbnDigits: '123456789X' >>> #(1 2 3 4 5 6 7 8 9 10).
	   self isbnDigits: '123456789' >>> #().
	   self isbnDigits: 'X123456789' >>> #().
	"
	| digits |

	digits := aString asArray
		select:      [ :c | c isDecimalDigit | (c = $X) ]
		thenCollect: [ :c | c = $X ifTrue: [ 10 ] ifFalse: [ c digitValue ] ].

	"note to self: 
		`Boolean >> and: aBlock` short-circuits if self is false.
		`Boolean >> & aBoolean`  does evaluate the right-hand side.
	"
	^ (digits size = 10 and: [ (digits allButLast includes: 10) not ])
		ifTrue: [ digits ]
		ifFalse: [ #() ]

]
