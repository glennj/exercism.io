Class {
	#name : #WordCount,
	#superclass : #Object,
	#category : #'Exercise@WordCount'
}

{ #category : #grouping }
WordCount >> countWordsSentence: aString [ 
	| wordPattern |
	"A word is some alphanumeric characters 
	 optionally followed by an apostrophe and some letters.
	
	 I'm relying on regex greediness to find the whole words
	 instead of using word anchors, which are inapplicable here
	 anyway because we're excluding underscore (which is a regex
	 word character) from this definition of a ""word""."
	wordPattern := '[[:alnum:]]+(''[[:alpha:]]+)?'.

	^(((
		aString allRegexMatches: wordPattern )
		groupedBy: #asLowercase )
		associations 
		collect: [ :each | each key -> each value size] ) 
		asDictionary
]
