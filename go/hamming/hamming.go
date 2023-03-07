package hamming

import (
	"errors"
	"unicode/utf8"
)

func Distance(a, b string) (int, error) {
	if utf8.RuneCountInString(a) != utf8.RuneCountInString(b) {
		return 0, errors.New("unequal lengths")
	}

	distance := 0
	for len(a) > 0 && len(b) > 0 {
		aRune, aRuneSize := utf8.DecodeRuneInString(a)
		bRune, bRuneSize := utf8.DecodeRuneInString(b)

		if aRune != bRune {
			distance++
		}

		a = a[aRuneSize:]
		b = b[bRuneSize:]
	}
	return distance, nil
}

/* a byte-oriented solution
import "errors"

func Distance(a, b string) (int, error) {
	if len(a) != len(b) {
		return 0, errors.New("unequal lengths")
	}

	distance := 0
	for i := 0; i < len(a); i++ {
		if a[i] != b[i] {
			distance++
		}
	}
	return distance, nil
}
*/

/* utf8 solution bench:
 * BenchmarkHamming         2301901               550.2 ns/op            64 B/op          4 allocs/op
 *
 * byte solution, unsurprisingly:
 * BenchmarkHamming        37587837                29.36 ns/op            0 B/op          0 allocs/op
 */
