import tables

const RelativeOrbits = {
   "Mercury":   0.2408467,
   "Venus":     0.61519726,
   "Earth":     1.0,
   "Mars":      1.8808158,
   "Jupiter":  11.862615,
   "Saturn":   29.447498,
   "Uranus":   84.016846,
   "Neptune": 164.79132,
}.toTable

const SecondsPerEarthYear = 31_557_600

proc age*(planet: string, ageInSeconds: BiggestInt): float =
  ageInSeconds.toBiggestFloat / SecondsPerEarthYear / RelativeOrbits[planet]

