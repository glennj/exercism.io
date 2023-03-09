package etl

import "strings"

func Transform(in map[int][]string) map[string]int {
	out := map[string]int{}

	for val, letters := range in {
		for _, letter := range letters {
			out[strings.ToLower(letter)] = val
		}
	}

	return out
}
