package cipher

import "strings"

type shift struct {
	Distance rune
}

func NewShift(distance int) Cipher {
	switch {
	case -25 <= distance && distance <= -1: return shift{Distance: rune(distance + 26)}
	case   1 <= distance && distance <= 25: return shift{Distance: rune(distance)}
	default:
		return nil
	}
}

func (c shift) Encode(input string) string {
	return c.encipher(+1, input)
}

func (c shift) Decode(input string) string {
	return c.encipher(-1, input)
}

func (c shift) encipher(direction int, input string) string {
	encipherRune := func(char rune) rune {
		if char < 'a' || char > 'z' {
			return -1
		}
		offset := char - 'a' + rune(direction)*c.Distance
		return 'a' + rune(floorMod(int(offset), 26))
	}

	return strings.Map(encipherRune, strings.ToLower(input))
}
