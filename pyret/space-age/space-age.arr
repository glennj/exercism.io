use context essentials2020 # Don't delete this line when using Pyret on Exercism 

provide: on-planet end

include string-dict

EarthYearsPerOrbit = [string-dict:
  'Mercury',   0.2408467,
  'Venus',     0.61519726,
  'Earth',     1.0,
  'Mars',      1.8808158,
  'Jupiter',  11.862615,
  'Saturn',   29.447498,
  'Uranus',   84.016846,
  'Neptune', 164.79132
]
SecondsPerEarthYear = 31557600

fun on-planet(planet, seconds):
  cases(Option) EarthYearsPerOrbit.get(planet):
    | none => raise("not a planet")
    | some(orbit) => (seconds / SecondsPerEarthYear) / orbit
  end
end
