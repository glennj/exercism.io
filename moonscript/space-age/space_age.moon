seconds_per_earth_year = 31557600

earth_years_per_orbit = {
  Mercury:   0.2408467,
  Venus:     0.61519726,
  Earth:     1.0,
  Mars:      1.8808158,
  Jupiter:  11.862615,
  Saturn:   29.447498,
  Uranus:   84.016846,
  Neptune: 164.79132,
}

{
  age: (planet, seconds) ->
    relative_year = earth_years_per_orbit[planet] or error 'not a planet'
    seconds / seconds_per_earth_year / relative_year
}
