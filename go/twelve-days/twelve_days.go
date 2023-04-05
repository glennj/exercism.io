package twelve

import (
	"fmt"
	"strings"
)

var (
	Ordinal = []string{
		"", "first", "second", "third", "fourth", "fifth", "sixth",
		"seventh", "eighth", "ninth", "tenth", "eleventh", "twelfth",
	}
	Gifts = []string{
		"",
		"a Partridge in a Pear Tree",
		"two Turtle Doves",
		"three French Hens",
		"four Calling Birds",
		"five Gold Rings",
		"six Geese-a-Laying",
		"seven Swans-a-Swimming",
		"eight Maids-a-Milking",
		"nine Ladies Dancing",
		"ten Lords-a-Leaping",
		"eleven Pipers Piping",
		"twelve Drummers Drumming",
	}
)

func Verse(n int) string {
	presents := make([]string, 0, n)
	for i := n; i > 0; i-- {
		conjunction := ""
		if i == 1 && n > 1 {
			conjunction = "and "
		}
		presents = append(presents, conjunction+Gifts[i])
	}

	return fmt.Sprintf("On the %s day of Christmas my true love gave to me: %s.",
		Ordinal[n],
		strings.Join(presents, ", "),
	)
}

func Song() string {
	n := len(Gifts) - 1
	verses := make([]string, 0, n)
	for i := 1; i <= n; i++ {
		verses = append(verses, Verse(i))
	}
	return strings.Join(verses, "\n")
}

// benchmarks
// BenchmarkVerse-2   	  181660	      6004 ns/op	    6192 B/op	      70 allocs/op
// BenchmarkSong-2    	  165778	      7081 ns/op	    9073 B/op	      72 allocs/op
