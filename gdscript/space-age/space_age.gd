const SECONDS_IN_EARTH_YEAR = 31557600


func on_planet(planet, age_in_seconds):

	var age_on_planet = func (earth_years_per_planet_year):
		return float(age_in_seconds) \
				/ SECONDS_IN_EARTH_YEAR \
				/ earth_years_per_planet_year

	match planet:
		"Mercury":
			return age_on_planet.call(0.2408467)
		"Venus":
			return age_on_planet.call(0.61519726)
		"Earth":
			return age_on_planet.call(1.0)
		"Mars":
			return age_on_planet.call(1.8808158)
		"Jupiter":
			return age_on_planet.call(11.862615)
		"Saturn":
			return age_on_planet.call(29.447498)
		"Uranus":
			return age_on_planet.call(84.016846)
		"Neptune":
			return age_on_planet.call(164.79132)

