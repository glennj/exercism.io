Extension { #name : #String }

{ #category : #'*Exercise@Isogram' }
String >> isIsogram [
	"Return true if every letter (case insensitively) appears exactly once in the string.
		'Hello' isIsogram  >>> false
		'LumberJacks' isIsogram >>> true
	"
	| seen |
	seen := Set new.
	self asLowercase
		select: #isLetter
		thenDo: [ :letter |
			seen
				addIfNotPresent: letter
				ifPresentDo: [ ^ false ]
		].
	^ true
		
]
