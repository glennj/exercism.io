package strand

import "strings"

func ToRNA(dna string) string {
	//return stringsToRNA(dna)
	//return builderToRNA(dna)
	return iterativeToRNA(dna)
}

// ------------------------------------------------------------
func stringsToRNA(dna string) string {
	mapper := func(nucleotide rune) rune {
		switch nucleotide {
		case 'G': return 'C'
		case 'C': return 'G'
		case 'T': return 'A'
		case 'A': return 'U'
		default:  panic("invalid nucleotide")
		}
	}

	return strings.Map(mapper, dna)
}

// ------------------------------------------------------------
var dna2rna = map[rune]rune{'G': 'C', 'C': 'G', 'T': 'A', 'A': 'U'}

func builderToRNA(dna string) string {
	rna := strings.Builder{}
	rna.Grow(len(dna))

	for _, nucleotide := range dna {
		r, ok := dna2rna[nucleotide]
		if !ok { panic("invalid nucleotide") }
		rna.WriteRune(r)
	}

	return rna.String()
}

func iterativeToRNA(dna string) string {
	rna := make([]rune, len(dna))

	for i, nucleotide := range dna {
		r, ok := dna2rna[nucleotide]
		if !ok { panic("invalid nucleotide") }
		rna[i] = r
	}

	return string(rna)
}

/* benchmarks
 *
 * strings.Map()
 * BenchmarkRNATranscription        3101014               385.4 ns/op            37 B/op          5 allocs/op
 *
 * iterative with a string Builder for output
 * BenchmarkRNATranscription        3546974               324.2 ns/op            16 B/op          5 allocs/op
 *
 * iterative with a slice for outut
 *
 */
