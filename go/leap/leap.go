// Package leap contains functions related to leap years.
package leap

// IsLeapYear determines if a given year is a leap year.
func IsLeapYear(year int) bool {
	// a little closure in place of a full `isDivisiblby(x,y)` function.
	remZero := func(n int) bool { return year % n == 0 }

	return remZero(4) && !remZero(100) || remZero(400)
}
