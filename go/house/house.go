package house

import "strings"

var (
	this = []string{
		"",
		"house that Jack built",
		"malt",
		"rat",
		"cat",
		"dog",
		"cow with the crumpled horn",
		"maiden all forlorn",
		"man all tattered and torn",
		"priest all shaven and shorn",
		"rooster that crowed in the morn",
		"farmer sowing his corn",
		"horse and the hound and the horn",
	}
	that = []string{
		"",
		"",
		"lay in",
		"ate",
		"killed",
		"worried",
		"tossed",
		"milked",
		"kissed",
		"married",
		"woke",
		"kept",
		"belonged to",
	}
)

func Verse(v int) string {
	verse := strings.Builder{}

	verse.WriteString("This is the " + this[v])
	for i := v; i > 1; i-- {
		verse.WriteString("\nthat " + that[i] + " the " + this[i-1])
	}
	verse.WriteRune('.')
	return verse.String()
}

func Song() string {
	verses := make([]string, 0, len(this))

	for v := 1; v < len(this); v++ {
		verses = append(verses, Verse(v))
	}
	return strings.Join(verses, "\n\n")
}

/* bench
 *
 * BenchmarkVerse-2   	  193957	      5865 ns/op	    7904 B/op	      80 allocs/op
 * BenchmarkSong-2    	  189272	      7217 ns/op	   10800 B/op	      82 allocs/op
 */
