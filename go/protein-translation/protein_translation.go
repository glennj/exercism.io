package protein

import "errors"

var ErrStop = errors.New("stop")
var ErrInvalidBase = errors.New("invalid codon")

func FromRNA(rna string) ([]string, error) {
	var codon string
	//var proteins []string
	proteins := make([]string, 0, len(rna)/3)

	for len(rna) >= 3 {
		codon, rna = rna[0:3], rna[3:]
		protein, err := FromCodon(codon)

		switch err {
		case nil:
			proteins = append(proteins, protein)
		case ErrStop:
			return proteins, nil
		default:
			return nil, err
		}
	}

	// TODO what if there are leftover characters?

	return proteins, nil
}

func FromCodon(codon string) (protein string, err error) {
	switch codon {
	case "AUG":
		protein = "Methionine"
	case "UUU", "UUC":
		protein = "Phenylalanine"
	case "UUA", "UUG":
		protein = "Leucine"
	case "UCU", "UCC", "UCA", "UCG":
		protein = "Serine"
	case "UAU", "UAC":
		protein = "Tyrosine"
	case "UGU", "UGC":
		protein = "Cysteine"
	case "UGG":
		protein = "Tryptophan"
	case "UAA", "UAG", "UGA":
		err = ErrStop
	default:
		err = ErrInvalidBase
	}

	return protein, err
}

/* benchmarks
 * BenchmarkCodon-2        65014584                18.56 ns/op            0 B/op          0 allocs/op
 *
 * using `var proteins []string`
 * BenchmarkProtein-2       1879693               612.4 ns/op           400 B/op         12 allocs/op
 *
 * using `proteins := make([]string, 0, len(rna)/3)` allocates fewer bytes per op
 * BenchmarkProtein-2       3751837               309.8 ns/op           352 B/op          5 allocs/op
 */
