package dna

import "fmt"

// Histogram is a mapping from nucleotide to its count in given DNA.
type Histogram map[byte]int

// DNA is a list of nucleotides. Choose a suitable data type.
type DNA []byte

// Counts generates a histogram of valid nucleotides in the given DNA.
// Returns an error if d contains an invalid nucleotide.
func (d DNA) Counts() (Histogram, error) {
	h := Histogram{'A': 0, 'C': 0, 'G': 0, 'T': 0}

	for _, nucleotide := range d {
		switch nucleotide {
		case 'A', 'C', 'G', 'T':
			h[nucleotide]++
		default:
			return nil, fmt.Errorf("invalid nucleotide: %c", nucleotide)
		}
	}

	return h, nil
}
