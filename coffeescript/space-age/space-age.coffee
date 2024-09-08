class SpaceAge
  secondsPerEarthYear = 31557600

  # a partial function
  planetaryAge = (earthYearsPerPlanetaryYear) -> 
    (seconds) -> seconds / secondsPerEarthYear / earthYearsPerPlanetaryYear
  
  @onMercury: (seconds) -> (planetaryAge   0.2408467 ) seconds 
  @onVenus: (seconds)   -> (planetaryAge   0.61519726) seconds 
  @onEarth: (seconds)   -> (planetaryAge   1.0       ) seconds
  @onMars: (seconds)    -> (planetaryAge   1.8808158 ) seconds
  @onJupiter: (seconds) -> (planetaryAge  11.862615  ) seconds
  @onSaturn: (seconds)  -> (planetaryAge  29.447498  ) seconds 
  @onUranus: (seconds)  -> (planetaryAge  84.016846  ) seconds
  @onNeptune: (seconds) -> (planetaryAge 164.79132   ) seconds

module.exports = SpaceAge
