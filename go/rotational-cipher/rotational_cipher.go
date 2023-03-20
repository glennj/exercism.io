package rotationalcipher

import "strings"

func RotationalCipher(plain string, shiftKey int) string {
	rotate := func(r rune, base rune) rune {
		return base + (r - base + rune(shiftKey))%26
	}

	rotator := func(in rune) (out rune) {
		switch {
            case 65 <= in && in <= 90:  out = rotate(in, 'A')
            case 97 <= in && in <= 122: out = rotate(in, 'a')
            default: out = in
		}
		return
	}

	return strings.Map(rotator, plain)
}

/* bench
 * BenchmarkRotationalCipher-2      2155501               550.6 ns/op           136 B/op          7 allocs/op
 */
