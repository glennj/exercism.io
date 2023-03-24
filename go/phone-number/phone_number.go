package phonenumber

//lint:file-ignore U1000 only one func will actually be used

import (
	"errors"
	"fmt"
	"regexp"
)

func Number(phoneNumber string) (number string, err error) {
	number, err = numberBasic(phoneNumber)
	//number, err = numberRegexp(phoneNumber)
	//number, err = numberRegexpVars(phoneNumber)
	return
}

// ------------------------------------------------------------
func phoneNumberError(reason string) error {
	return errors.New("invalid phone number: " + reason)
}

var (
	ErrTooLong      = phoneNumberError("too long")
	ErrTooShort     = phoneNumberError("too short")
	ErrCountryCode  = phoneNumberError("invalid country code")
	ErrAreaCode     = phoneNumberError("invalid area code")
	ErrExchangeCode = phoneNumberError("invalid exchange code")
)

func numberBasic(phoneNumber string) (string, error) {
	digits := make([]rune, 0, 11)
	for _, c := range phoneNumber {
		if '0' <= c && c <= '9' {
			digits = append(digits, c)
		}
	}

	switch len(digits) {
	case 10:
	case 11:
		if digits[0] != '1' {
			return "", ErrCountryCode
		}
		digits = digits[1:]
	case 0, 1, 2, 3, 4, 5, 6, 7, 8, 9:
		return "", ErrTooShort
	default:
		return "", ErrTooLong
	}

	if digits[0] == '0' || digits[0] == '1' {
		return "", ErrAreaCode
	}
	if digits[3] == '0' || digits[3] == '1' {
		return "", ErrExchangeCode
	}

	return string(digits), nil
}

// ------------------------------------------------------------

func numberRegexp(phoneNumber string) (string, error) {
	digits := regexp.MustCompile(`\D`).ReplaceAllLiteralString(phoneNumber, "")
	if !regexp.MustCompile(`^1?([2-9][0-9]{2}){2}[0-9]{4}$`).MatchString(digits) {
		return "", errors.New("invalid phone number")
	}
	return regexp.MustCompile(`^1`).ReplaceAllLiteralString(digits, ""), nil
}

// ------------------------------------------------------------
var (
	nonDigit    = regexp.MustCompile(`\D`)
	validNANP   = regexp.MustCompile(`^1?(?:[2-9][0-9]{2}){2}[0-9]{4}$`)
	countryCode = regexp.MustCompile(`^1`)
	ErrInvalid  = errors.New("invalid phone number")
)

func numberRegexpVars(phoneNumber string) (string, error) {
	digits := nonDigit.ReplaceAllLiteralString(phoneNumber, "")
	if !validNANP.MatchString(digits) {
		return "", ErrInvalid
	}
	return countryCode.ReplaceAllLiteralString(digits, ""), nil
}

// ------------------------------------------------------------
func AreaCode(phoneNumber string) (string, error) {
	number, err := Number(phoneNumber)
	if err != nil {
		return "", fmt.Errorf("cannot get area code: %w", err)
	}
	return number[0:3], nil
}

func Format(phoneNumber string) (string, error) {
	number, err := Number(phoneNumber)
	if err != nil {
		return "", fmt.Errorf("cannot format number: %w", err)
	}
	return fmt.Sprintf("(%s) %s-%s", number[0:3], number[3:6], number[6:]), nil
}

/* benchmarks
 *
 * using "basic" string/rune manipulation
 * BenchmarkNumber-2        1398189               880.6 ns/op           176 B/op          6 allocs/op
 * BenchmarkAreaCode-2       302790              3859 ns/op            1488 B/op         32 allocs/op
 * BenchmarkFormat-2         207187              4968 ns/op            1808 B/op         52 allocs/op
 *
 * using regexp, orders of magnitude slower to create the re's in the func
 * BenchmarkNumber-2           7094            175721 ns/op          164115 B/op       1977 allocs/op
 * BenchmarkAreaCode-2         6441            186501 ns/op          165198 B/op       2003 allocs/op
 * BenchmarkFormat-2           6380            172581 ns/op          165510 B/op       2023 allocs/op
 *
 * using regexp, create the re's as package vars
 * BenchmarkNumber-2          64564             16194 ns/op            1171 B/op         81 allocs/op
 * BenchmarkAreaCode-2        60267             19328 ns/op            2212 B/op        107 allocs/op
 * BenchmarkFormat-2          58489             20536 ns/op            2533 B/op        127 allocs/op
*/
