package scrabble

import "strings"

var tiles = map[rune]int{
	'A': 1, 'E': 1, 'I': 1, 'O': 1, 'U': 1,
	'L': 1, 'N': 1, 'R': 1, 'S': 1, 'T': 1,
	'D': 2, 'G': 2,
	'B': 3, 'C': 3, 'M': 3, 'P': 3,
	'F': 4, 'H': 4, 'V': 4, 'W': 4, 'Y': 4,
	'K': 5,
	'J': 8, 'X': 8,
	'Q': 10, 'Z': 10,
}

func tile(r rune) int {
	switch r {
	case 'A', 'E', 'I', 'O', 'U': return 1
	case 'L', 'N', 'R', 'S', 'T': return 1
	case 'D', 'G':                return 2
	case 'B', 'C', 'M', 'P':      return 3
	case 'F', 'H', 'V', 'W', 'Y': return 4
	case 'K':                     return 5
	case 'J', 'X':                return 8
	case 'Q', 'Z':                return 10
	default:                      return 0
	}
}

func Score(word string) int {
	var score int
	for _, r := range strings.ToUpper(word) {
		//score += tiles[r]
		score += tile(r)
	}
	return score
}

/* benchmarking
 *
 * with a map
 * BenchmarkScore    898912              1221 ns/op              80 B/op          9 allocs/op
 *
 * with a switch
 * BenchmarkScore   1357786               913.2 ns/op            80 B/op          9 allocs/op
 */
