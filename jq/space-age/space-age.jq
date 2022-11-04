# input: a floating point number
# output: the number rounded to two decimal places
def two_decimal: ((. * 100) | round) / 100;

{
  "Mercury":   0.2408467,
  "Venus":     0.61519726,
  "Earth":     1.0,
  "Mars":      1.8808158,
  "Jupiter":  11.862615,
  "Saturn":   29.447498,
  "Uranus":   84.016846,
  "Neptune": 164.79132
} as $earth_years_per_planet_year
| 31557600 as $seconds_per_earth_year

| if .planet | in($earth_years_per_planet_year) | not
  then "not a planet" | halt_error
  else 
    .seconds / $seconds_per_earth_year / $earth_years_per_planet_year[.planet]
    | two_decimal
  end
