Class {
	#name : #TwelveDays,
	#superclass : #Object,
	#category : #'Exercise@TwelveDays'
}

{ #category : #accessing }
TwelveDays class >> days [
	"ordinal name for each of the 12 days"

	^ #(
	   'first' 'second' 'third' 'fourth' 'fifth' 'sixth'
 	  'seventh' 'eighth' 'ninth' 'tenth' 'eleventh' 'twelfth'
	).
]

{ #category : #'as yet unclassified' }
TwelveDays class >> gifts [
	"list of gifts for each day"

	^ #(
	   'a Partridge in a Pear Tree'
 	   'two Turtle Doves'
	   'three French Hens'
	   'four Calling Birds'
	   'five Gold Rings'
	   'six Geese-a-Laying'
	   'seven Swans-a-Swimming'
	   'eight Maids-a-Milking'
	   'nine Ladies Dancing'
	   'ten Lords-a-Leaping'
	   'eleven Pipers Piping'
	   'twelve Drummers Drumming'
	).
]

{ #category : #'as yet unclassified' }
TwelveDays >> reciteStartVerse: startDay endVerse: endDay [
	^ (startDay to: endDay) collect: [ :day | self verse: day ].
]

{ #category : #'as yet unclassified' }
TwelveDays >> verse: dayNum [
	| days gifts |
	
	days := self class days.
	(dayNum between: 1 and: days size)
		ifFalse: [ self error: 'Invalid day: ', dayNum ].
	
	gifts := (dayNum to: 1 by: -1) collect: [ :i | self class gifts at: i ].
	(dayNum > 1)
		ifTrue: [ gifts at: dayNum put: ('and ' , (gifts at: dayNum)) ].

	^ 'On the {day} day of Christmas my true love gave to me: {giftList}.'
		format: 
			(Dictionary
				with: #day      -> (days at: dayNum)
				with: #giftList -> (', ' join: gifts)).
]
