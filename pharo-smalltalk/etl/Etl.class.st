Class {
	#name : #Etl,
	#superclass : #Object,
	#category : #'Exercise@Etl'
}

{ #category : #transforming }
Etl >> transformData: inputDictionary [
	^(inputDictionary 
		associations
		flatCollect: [ :association |
			| score |
			score := association key asInteger.
			association value 
				collect: [ :letter | (letter asLowercase) -> score ] ] )
	asDictionary
]

{ #category : #transforming }
Etl >> transformData_keysAndValuesDo: inputDictionary [
	| result |
	result := Dictionary new.
	
	inputDictionary keysAndValuesDo: [ :score :tilesList |
		tilesList do: [ :tile |
			result 
				at: tile asLowercase 
				put: score asInteger ] ].
	^ result
]
