package romannumerals

import "fmt"

func ToRomanNumeral(input int) (roman string, err error) {
	if input < 1 || input > 3999 {
		err = fmt.Errorf("%d is roman of range", input)
	} else {
		for input >= 1000 {
			roman += "M"
			input -= 1000
		}
		if input >= 900 {
			roman += "CM"
			input -= 900
		}
		if input >= 500 {
			roman += "D"
			input -= 500
		}
		if input >= 400 {
			roman += "CD"
			input -= 400
		}
		for input >= 100 {
			roman += "C"
			input -= 100
		}
		if input >= 90 {
			roman += "XC"
			input -= 90
		}
		if input >= 50 {
			roman += "L"
			input -= 50
		}
		if input >= 40 {
			roman += "XL"
			input -= 40
		}
		for input >= 10 {
			roman += "X"
			input -= 10
		}
		if input >= 9 {
			roman += "IX"
			input -= 9
		}
		if input >= 5 {
			roman += "V"
			input -= 5
		}
		if input >= 4 {
			roman += "IV"
			input -= 4
		}
		for input >= 1 {
			roman += "I"
			input -= 1
		}
	}

	return roman, err
}

/* bench
 * BenchmarkRomanNumerals-2          543532              1967 ns/op             304 B/op         65 allocs/op
 */
