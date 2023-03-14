package reverse

import (
	"unicode/utf8"
)

func Reverse(input string) string {
	//return rangeIteration(input)
	return inplaceRuneSlice(input)
	//return concatRunes(input)
	//return decodingLastRune(input)
}

func rangeIteration(input string) string {
	n := utf8.RuneCountInString(input)
	reversed := make([]rune, n)

	for _, r := range input {
		n--
		reversed[n] = r
	}

	return string(reversed)
}

func inplaceRuneSlice(input string) string {
	rs := []rune(input)
	for i, j := 0, len(rs)-1; i < j; i, j = i+1, j-1 {
		rs[i], rs[j] = rs[j], rs[i]
	}
	return string(rs)
}

func concatRunes(input string) string {
	var reversed []rune
	for _, r := range input {
		reversed = append([]rune{r}, reversed...)
	}
	return string(reversed)
}

func decodingLastRune(input string) string {
	var reversed []rune

	for input != "" {
		r, size := utf8.DecodeLastRuneInString(input)
		reversed = append(reversed, r)
		input = input[:len(input) - size]

	}
	return string(reversed)
}

/* benchmark
 *
 * range iteration
 * BenchmarkReverse         1595744               698.1 ns/op           216 B/op         11 allocs/op
 *
 * inplace reverse of rune slice
 * BenchmarkReverse         3320209               387.3 ns/op             0 B/op          0 allocs/op
 *
 * concatenating rune slices, lots of array building
 * BenchmarkReverse          418412              2517 ns/op             784 B/op         63 allocs/op
 *
 * using DecodeLastRune
 * BenchmarkReverse          962007              1232 ns/op             416 B/op         22 allocs/op
 */
