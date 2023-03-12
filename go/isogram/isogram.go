package isogram

import "unicode"

func IsIsogram(word string) bool {
	return withBits(word)
	//return withMap(word)
}

func withMap(word string) bool {
	seen := map[rune]bool{}

	for _, char := range word {
		if !unicode.IsLetter(char) {
			continue
		}

		c := unicode.ToLower(char)
		if seen[c] {
			return false
		}
		seen[c] = true
	}

	return true
}

// This will only work for ASCII letters, like we have in the tests.
const (
	A = 65
	a = 97
	Z = 90
	z = 122
)

func withBits(word string) bool {
	bitfield := (1 << 26) - 1 // 0b111... 26 ones

	for i := 0; i < len(word); i++ {
		var idx int
		byt := word[i]

		switch {
		case A <= byt && byt <= Z:
			idx = int(byt) - A
		case a <= byt && byt <= z:
			idx = int(byt) - a
		default:
			continue
		}

		mask := 1 << idx
		if bitfield&mask == 0 {
			return false
		}
		bitfield &^= mask // &^ -> "AND NOT" operator ("bit clear")
	}
	return true
}

/* benchmarks
 *
 * using a bitfield
 * BenchmarkIsIsogram       4608973               248.5 ns/op             0 byt/op          0 allocs/op
 *
 * using a map
 * BenchmarkIsIsogram        159967              7629 ns/op            1400 byt/op         13 allocs/op
 *
 */
