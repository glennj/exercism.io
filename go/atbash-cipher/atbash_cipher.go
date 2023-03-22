package atbash

//lint:file-ignore U1000 only one func will actually be used

import (
	"fmt"
	"regexp"
	"strings"
)

func Atbash(s string) string {
	encoded := strings.Map(transliterate, s)
	return grouped(encoded, 5)
}

func transliterate(c rune) rune {
	switch {
	case 'A' <= c && c <= 'Z':
		return 'a' + 'Z' - c
	case 'a' <= c && c <= 'z':
		return 'a' + 'z' - c
	case '0' <= c && c <= '9':
		return c
	default:
		return -1
	}
}

func grouped(s string, size int) string {
	return groupedBytes(s, size)
	//return groupedRunes(s, size)
	//return groupedRegex(s, size)
}

// This operates on bytes, not runes.
// This is OK: the transliterate function filters out non-ASCII chars.
func groupedBytes(s string, size int) string {
	groups := make([]string, 0, (len(s)/size + 1))

	for len(s) >= size {
		groups = append(groups, s[:size])
		s = s[size:]
	}

	if len(s) > 0 {
		groups = append(groups, s)
	}

	return strings.Join(groups, " ")
}

func groupedRunes(s string, size int) string {
	var (
		groups = make([]string, 0, (len(s)/size + 1))
		group  = strings.Builder{}
		count  = 0
	)

	for _, r := range s {
		group.WriteRune(r)
		count++

		if count == size {
			groups = append(groups, group.String())
			group.Reset()
			count = 0
		}
	}

	if group.Len() > 0 {
		groups = append(groups, group.String())
	}

	return strings.Join(groups, " ")
}

func groupedRegex(s string, size int) string {
	re := regexp.MustCompile(fmt.Sprintf(".{1,%d}", size))
	addSpace := func(g string) string { return g + " " }
	grouped := re.ReplaceAllStringFunc(s, addSpace)
	return strings.TrimSpace(grouped)
}

/* bench
 *
 * byte-oriented grouped function
 * BenchmarkAtbash-2         861531              1391 ns/op             616 B/op         20 allocs/op
 *
 * rune-oriented grouped function
 * BenchmarkAtbash-2         506740              2367 ns/op             792 B/op         41 allocs/op
 *
 * regexp grouping
 * BenchmarkAtbash-2          44876             24535 ns/op           23313 B/op        261 allocs/op
 */
