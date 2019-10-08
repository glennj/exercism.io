Class {
	#name : #SpaceAge,
	#superclass : #Object,
	#classVars : [
		'RotationalPeriod',
		'SecondsPerEarthYear'
	],
	#category : #'Exercise@SpaceAge'
}

{ #category : #'class initialization' }
SpaceAge class >> initialize [
	SecondsPerEarthYear ifNil: [SecondsPerEarthYear := 31557600.].

   "The length of each planetary orbit, in Earth years"
	RotationalPeriod ifNil: [
		RotationalPeriod := Dictionary newFromPairs: { 
			'Mercury' .   0.2408467  .
			'Venus'   .   0.61519726 .
			'Earth'   .   1.0        .
			'Mars'    .   1.8808158  .
			'Jupiter' .  11.862615   .
			'Saturn'  .  29.447498   .
			'Uranus'  .  84.016846   .
			'Neptune' . 164.79132    } ]
]

{ #category : #'as yet unclassified' }
SpaceAge >> ageOnPlanet: planet at: ageInSeconds [ 
	^ ageInSeconds / SecondsPerEarthYear / (RotationalPeriod at: planet)
]

{ #category : #initialization }
SpaceAge >> initialize [
	super initialize.
	self class initialize.

]
