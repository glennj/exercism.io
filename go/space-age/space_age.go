package space

type Planet string

const secondsPerEarthYear = 31557600.0

func Age(seconds float64, planet Planet) float64 {
	return switchAge(seconds, planet)
	//return mapAge(seconds, planet)
	//return mapRedefineAge(seconds, planet)
}

// -----------------------------------------------------
func switchAge(seconds float64, planet Planet) float64 {
	var earthYearsPerOrbit float64
	switch planet {
	case "Mercury":
		earthYearsPerOrbit = 0.2408467
	case "Venus":
		earthYearsPerOrbit = 0.61519726
	case "Earth":
		earthYearsPerOrbit = 1.0
	case "Mars":
		earthYearsPerOrbit = 1.8808158
	case "Jupiter":
		earthYearsPerOrbit = 11.862615
	case "Saturn":
		earthYearsPerOrbit = 29.447498
	case "Uranus":
		earthYearsPerOrbit = 84.016846
	case "Neptune":
		earthYearsPerOrbit = 164.79132
	default:
		return -1
	}
	return seconds / secondsPerEarthYear / earthYearsPerOrbit
}

// -----------------------------------------------------
var relativeOrbit = map[Planet]float64{
	"Mercury": 0.2408467,
	"Venus":   0.61519726,
	"Earth":   1.0,
	"Mars":    1.8808158,
	"Jupiter": 11.862615,
	"Saturn":  29.447498,
	"Uranus":  84.016846,
	"Neptune": 164.79132,
}

func mapAge(seconds float64, planet Planet) float64 {
	earthYearsPerOrbit, exists := relativeOrbit[planet]
	if !exists {
		return -1
	}
	return seconds / secondsPerEarthYear / earthYearsPerOrbit
}


// -----------------------------------------------------
func mapRedefineAge(seconds float64, planet Planet) float64 {
	var relativeOrbit = map[Planet]float64{
		"Mercury": 0.2408467,
		"Venus":   0.61519726,
		"Earth":   1.0,
		"Mars":    1.8808158,
		"Jupiter": 11.862615,
		"Saturn":  29.447498,
		"Uranus":  84.016846,
		"Neptune": 164.79132,
	}

	earthYearsPerOrbit, exists := relativeOrbit[planet]
	if !exists {
		return -1
	}
	return seconds / secondsPerEarthYear / earthYearsPerOrbit
}

/* benchmarks
 *
 * switch
 * BenchmarkAge    67883024                17.93 ns/op            0 B/op          0 allocs/op
 *
 * map
 * BenchmarkAge     9968689               118.6 ns/op             0 B/op          0 allocs/op
 *
 * map defined in func
 * BenchmarkAge      708831              1688 ns/op               0 B/op          0 allocs/op
 */
