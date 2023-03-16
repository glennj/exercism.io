package proverb

import "fmt"

const line = "For want of a %s the %s was lost."
const lastLine = "And all for the want of a %s."

// Proverb should have a comment documenting it. Yep.
func Proverb(rhyme []string) []string {
	if len(rhyme) == 0 {
		return nil
	}

	proverb := make([]string, len(rhyme))
	i := 0
	for i < len(rhyme)-1 {
		proverb[i] = fmt.Sprintf(line, rhyme[i], rhyme[i+1])
		i++
	}
	proverb[i] = fmt.Sprintf(lastLine, rhyme[0])

	return proverb
}

/* benchmark
 * BenchmarkProverb          186688              6360 ns/op            1472 B/op         51 allocs/op
 */
