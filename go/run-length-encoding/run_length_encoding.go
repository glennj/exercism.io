package encode


import (
	"fmt"
	"strings"
	"regexp"
	"strconv"
	"unicode"
	"unicode/utf8"
)

// ------------------------------------------------------------
func RunLengthEncode(input string) string {
	prev, size := utf8.DecodeRuneInString(input)
	if size == 0 {
		return ""
	}

	count := 0
	out := strings.Builder{}

	encode := func() {
		if count == 1 {
			out.WriteRune(prev)
		} else {
			out.WriteString(fmt.Sprintf("%d%c", count, prev))
		}
	}

	for _, char := range input {
		if char == prev {
			count++
		} else {
			encode()
			count = 1
			prev = char
		}
	}
	encode()

	return out.String()
}

// Go regexes don't support backreferences.
// Shame, that's such an elegant solution

// ------------------------------------------------------------
func RunLengthDecode(input string) string {
	return iteratingRunLengthDecode(input)
	//return regexpRunLengthDecode(input)
}

func iteratingRunLengthDecode(input string) string {
	var count int
	out := strings.Builder{}

	for _, char := range input {
		if unicode.IsDigit(char) {
			count = 10 * count + int(char - '0')
		} else {
			if count == 0 {
				out.WriteRune(char)
			} else {
				out.WriteString(strings.Repeat(string(char), count))
			}
			count = 0
		}
	}
	return out.String()
}

// less performant but more compact.
func regexpRunLengthDecode(input string) string {
	replacer := func(match string) string {
		chars := []rune(match)
		digits, char := chars[:len(chars)-1], chars[len(chars)-1]
		count, _ := strconv.Atoi(string(digits))
		return strings.Repeat(string(char), count)
	}
	re := regexp.MustCompile(`\d+\D`)

	return re.ReplaceAllStringFunc(input, replacer)
}

/* benchmark
 *
 * encode
 * BenchmarkRunLengthEncode          434499              2890 ns/op             120 B/op         21 allocs/op
 *
 * iterating decode
 * BenchmarkRunLengthDecode          661233              1632 ns/op             288 B/op         24 allocs/op
 *
 * regexp decode
 * BenchmarkRunLengthDecode           70599             18398 ns/op            9336 B/op        145 allocs/op
 */
