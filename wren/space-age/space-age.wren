class Space {
  static initialize_() {
    __secondsPerEarthYear = 31557600
    __earthYearsPerOrbit = {
      "mercury":    0.2408467,
      "venus":      0.61519726,
      "earth":      1.0,
      "mars":       1.8808158,
      "jupiter":   11.862615,
      "saturn":   29.447498,
      "uranus":   84.016846,
      "neptune": 164.79132,
    }
  }

  static age(planet, ageInSeconds) {
    if (!__earthYearsPerOrbit.containsKey(planet)) {
      Fiber.abort("not a planet")
    }
    var planetaryYears = ageInSeconds /
      __secondsPerEarthYear /
      __earthYearsPerOrbit[planet]
    return NumUtils.roundTo(planetaryYears, 2)
  }
}

Space.initialize_()

class NumUtils {
  static roundTo(num, precision) {
    var m = 10.pow(precision)
    return (num * m).round / m
  }
}
