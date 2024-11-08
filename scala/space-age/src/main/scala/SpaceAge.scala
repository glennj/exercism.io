object SpaceAge {
  private val secondsPerEarthYear = 31_557_600.0
  
  private def spaceAge(earthYearsPerPlanetYear: Double)(ageInSeconds: Double): Double =
    ageInSeconds / secondsPerEarthYear / earthYearsPerPlanetYear

  // "partially-applied functions"
  val onMercury = spaceAge(0.2408467)(_)
  val onVenus   = spaceAge(0.61519726)(_)
  val onEarth   = spaceAge(1.0)(_)
  val onMars    = spaceAge(1.8808158)(_)
  val onJupiter = spaceAge(11.862615)(_)
  val onSaturn  = spaceAge(29.447498)(_)
  val onUranus  = spaceAge(84.016846)(_)
  val onNeptune = spaceAge(164.79132)(_)
}
