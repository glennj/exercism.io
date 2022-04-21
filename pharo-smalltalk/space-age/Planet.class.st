"
I am Planets
"
Class {
	#name : #Planet,
	#superclass : #Object,
	#category : #'Exercise@SpaceAge'
}

{ #category : #data }
Planet class >> earthYearsPerOrbit [
	self subclassResponsibility 
]

{ #category : #fetching }
Planet class >> get: planetName [
	^ self environment
		at: (planetName asSymbol)
		ifAbsent: [ self error: 'Unknown planet' ]
]

{ #category : #testing }
Planet class >> isAbstract [
		
	^ self == Planet
]
