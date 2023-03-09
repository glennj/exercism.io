package pangram

import "strings"

const alphabetSize = 26

/* my iteration 1
func IsPangram(input string) bool {
	lower := strings.ToLower(input)
	seen := make(map[rune]bool, alphabetSize)
	for _, char := range lower {
		if 'a' <= char && char <= 'z' {
			seen[char] = true
		}
		// early return maybe
		if len(seen) == alphabetSize {
			return true
		}
	}
	return false
}
*/

// now, having seen the "dig deeper", bit fields

const everyLetter = (1 << alphabetSize) - 1 // 0b111... 26 ones
const A = 0x41
const Z = 0x5A

func IsPangram(input string) bool {
	var seen int
	upper := strings.ToUpper(input)

	for i := 0; i < len(upper); i++ {
		byt := upper[i]
		if A <= byt && byt <= Z {
			seen |= 1 << (byt - A)
		}
		if seen == everyLetter {
			return true
		}
	}
	return false
}

/*************************************************************
 * benchmarks:
 *
 * my iteration 1
 * BenchmarkPangram           94566             11943 ns/op            2927 B/op         32 allocs/op
 *
 * the bitfield function
 * BenchmarkPangram          461461              2560 ns/op             416 B/op          9 allocs/op
 */

/*************************************************************
 * iteration 1 alternatives that are a bit slower
 *
 * not creating the `seen` map with a capacity:
 *		seen := map[rune]bool{}
 *
 * iterating over the _byte indices_ of the string
 *		seen := make(map[byte]bool, alphabetSize)
 *		for i := 0; i < len(lower); i++ {
 *			char := lower[i]
 *			if 0x61 <= char && char <= 0x7A {
 *				seen[char] = trye
 *			// ...
 *
 * deleting the char from an alphabet string
 *		alphabet := "abcdefghijklmnopqrstuvwxyz"
 *		//...
 *			if 'a' <= char && char <= 'z' {
 *				alphabet = strings.Replace(alphabet, string(char), "", 1)
 *			}
 *			if len(alphabet) == 0 {
 *				return true
 *			}
 *
 * deleting the char from a prepopulated map
 *		seen := map[rune]bool{
 *			'a': true,
 *			'b': true,
 *			//...
 *			'z': true,
 *		}
 *		//...
 *			if 'a' <= char && char <= 'z' {
 *				delete(seen, char)
 *			}
 *			if len(seen) == 0 {
 *				return true
 *			}
 */
