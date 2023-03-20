package anagram

import (
	"sort"
	"strings"
)

func Detect(subject string, candidates []string) (anagrams []string) {
	subjLower := strings.ToLower(subject)
	subjKey := anagramKey(subjLower)

	for _, candidate := range candidates {
		candLower := strings.ToLower(candidate)
        candKey := anagramKey(candLower)

		if candLower != subjLower && candKey == subjKey {
			anagrams = append(anagrams, candidate)
		}
	}

	return anagrams
}

// ------------------------------------------------------------
func anagramKey(word string) string {
	codepoints := make(sort.IntSlice, 0, len(word))
	for _, char := range word {
		codepoints = append(codepoints, int(char))
	}
	sort.Sort(codepoints)

	key := strings.Builder{}
	for _, cp := range codepoints {
		key.WriteRune(rune(cp))
	}
	return key.String()

	/*
	   runes := []rune(word)
	   sort.Slice(runes, func(i, j int) bool {return runes[i] < runes[j]})
	   return string(runes)
	*/
}

/* bench
 *
 * using the sort.IntSlice array of codepoints
 * BenchmarkDetectAnagrams-2          78528             15321 ns/op            5776 B/op        218 allocs/op
 *
 * using the more compact sort.Slice on an array of runes
 * BenchmarkDetectAnagrams-2          64156             18154 ns/op            6216 B/op        266 allocs/op
 */
