"
I can translate Earth years to years on another planet.
"
Class {
	#name : #SpaceAge,
	#superclass : #Object,
	#category : #'Exercise@SpaceAge'
}

{ #category : #converting }
SpaceAge >> ageOnPlanet: planet at: ageInSeconds [
	^ ageInSeconds / Earth secondsPerYear / (Planet get: planet) earthYearsPerOrbit
]
