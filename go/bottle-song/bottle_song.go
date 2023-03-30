package bottlesong

// Reusing my Say code: https://exercism.org/tracks/go/exercises/say/solutions/glennj

import (
	"fmt"
	"say"
	"strings"
	"unicode"
	"unicode/utf8"
)

const (
	bottle = "green bottle"
	where  = "hanging on the wall"
)

func Recite(startBottles, takeDown int) []string {
	song := make([]string, 0, 5*takeDown-1)
	for i := 0; i < takeDown; i++ {
		song = append(song, "")
		song = append(song, verse(startBottles-i)...)
	}
	return song[1:]
}

func verse(n int) []string {
	lines := []string{
		fmt.Sprintf("%s %s %s,", toSentenceCase(number(n)), bottles(n), where),
		"",
		fmt.Sprintf("And if one %s should accidentally fall,", bottles(1)),
		fmt.Sprintf("There'll be %s %s %s.", number(n-1), bottles(n-1), where),
	}
	lines[1] = lines[0]
	return lines
}

func number(n int) (word string) {
	word = "no"
	if n > 0 {
		word, _ = say.Say(int64(n))
	}
	return
}

func toSentenceCase(s string) string {
	r, size := utf8.DecodeRuneInString(s)
	return string(unicode.ToUpper(r)) + strings.ToLower(s[size:])
}

func bottles(n int) (b string) {
	b = bottle
	if n != 1 {
		b += "s"
	}
	return
}
