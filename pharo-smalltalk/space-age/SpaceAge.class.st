"
I can translate Earth years to years on another planet.
"
Class {
	#name : #SpaceAge,
	#superclass : #Object,
	#category : #'Exercise@SpaceAge'
}

{ #category : #constants }
SpaceAge class >> rotationalPeriod [ 
	"The length of each planetary orbit, in Earth years"
	^ { 
		'Mercury' ->   0.2408467  .
		'Venus'   ->   0.61519726 .
		'Earth'   ->   1.0        .
		'Mars'    ->   1.8808158  .
		'Jupiter' ->  11.862615   .
		'Saturn'  ->  29.447498   .
		'Uranus'  ->  84.016846   .
		'Neptune' -> 164.79132 
	} asDictionary
]

{ #category : #constants }
SpaceAge class >> secondsPerEarthYear [ 
	^ 31557600
]

{ #category : #converting }
SpaceAge >> ageOnPlanet: planet at: ageInSeconds [ 
	^ ageInSeconds / (self class secondsPerEarthYear) / (self class rotationalPeriod at: planet)
]
