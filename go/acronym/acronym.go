// Package acronym should have a package comment that summarizes what it's about.
package acronym

import (
	"strings"
	"unicode"
)

// a couple of states for the state machine conditions
const (
	inWord int = iota
	seekingWord
)

// Abbreviate should have a comment documenting it.
func Abbreviate(s string) string {
	abbr := strings.Builder{}
	state := seekingWord

	for _, c := range s {
		switch {
		case state == seekingWord && unicode.IsLetter(c):
			abbr.WriteRune(unicode.ToUpper(c))
			state = inWord
		case state == inWord && !(unicode.IsLetter(c) || c == '\''):
			state = seekingWord
		}
	}

	return abbr.String()
}

// bench
// BenchmarkAcronym-2        881917              1245 ns/op              88 B/op         10 allocs/op
