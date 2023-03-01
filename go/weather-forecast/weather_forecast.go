/* Package weather provides functionality to format a
 * weather forecast for a city.
 */
package weather

// CurrentCondition holds the current weather conditions.
var CurrentCondition string

// CurrentLocation holds the current city.
var CurrentLocation string

// Forecast takes two string parameters, a city and the conditions.
// The functions returns the weather forecast as a string.
func Forecast(city, condition string) string {
	CurrentLocation, CurrentCondition = city, condition
	return CurrentLocation + " - current weather condition: " + CurrentCondition
}
