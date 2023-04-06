package scale

import (
	"strings"

	"golang.org/x/exp/slices"
)

var (
	ChromaticSharps = []string{"A", "A#", "B", "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#"}
	ChromaticFlats  = []string{"A", "Bb", "B", "C", "Db", "D", "Eb", "E", "F", "Gb", "G", "Ab"}
	Flats           = []string{"F", "Bb", "Eb", "Ab", "Db", "Gb", "d", "g", "c", "f", "bb", "eb"}
)

func Scale(tonic, interval string) []string {
	var chromatic, scale []string

	if slices.Contains(Flats, tonic) {
		chromatic = ChromaticFlats
	} else {
		chromatic = ChromaticSharps
	}

	ucFirst := func(s string) string {
		return strings.ToUpper(string(s[0])) + strings.ToLower(s[1:])
	}

	idx := slices.Index(chromatic, ucFirst(tonic))
	if idx == -1 {
		panic("unknown tonic: " + tonic)
	}

	scale = make([]string, 0, len(chromatic))
	scale = append(scale, chromatic[idx:]...)
	scale = append(scale, chromatic[:idx]...)

	return applyInterval(scale, interval)
}

func applyInterval(scale []string, interval string) []string {
	if len(interval) == 0 {
		interval = "mmmmmmmmmmm"
	}

	result := make([]string, 0, len(scale))
	idx := 0
	for _, i := range interval {
		result = append(result, scale[idx])
		idx = (idx + intervalValue(i)) % len(scale)
	}
	return append(result, scale[idx])
}

func intervalValue(i rune) int {
	switch i {
	case 'm':
		return 1
	case 'M':
		return 2
	case 'A':
		return 3
	default:
		panic("invalid interval" + string(i))
	}
}

// bench
// BenchmarkScale-2   	  196246	      5936 ns/op	    6554 B/op	      53 allocs/op
