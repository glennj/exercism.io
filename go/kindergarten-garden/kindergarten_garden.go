package kindergarten

import (
	"fmt"
	"sort"
	"strings"

	"golang.org/x/exp/slices"
)

type Garden map[string][]string

func NewGarden(diagram string, children []string) (*Garden, error) {
	kids := slices.Clone(children)
	sort.Strings(kids)

	garden := Garden{}
	for _, child := range kids {
		garden[child] = make([]string, 4)
	}
	if len(garden) < len(kids) {
		return nil, fmt.Errorf(`children list contains duplicates`)
	}

	err := garden.parseInput(diagram, kids)
	if err != nil {
		return nil, fmt.Errorf("can't parse input: %w", err)
	}
	return &garden, nil
}

func (g *Garden) Plants(child string) (plants []string, ok bool) {
	plants, ok = (*g)[child]
	return
}

func (g *Garden) parseInput(diagram string, children []string) error {
	rows := strings.Split(diagram, "\n")[1:]
	if len(rows) != 2 || len(rows[0]) != len(rows[1]) || len(rows[0])%2 != 0 {
		return fmt.Errorf(`malformed input diagram`)
	}

	for i := 0; i < len(rows[0])/2; i++ {
		child := children[i]
		for j, code := range []byte{
			rows[0][2*i],
			rows[0][2*i+1],
			rows[1][2*i],
			rows[1][2*i+1],
		} {
			plant, err := lookupPlant(code)
			if err != nil {
				return fmt.Errorf("error parsing diagram: %w", err)
			}
			(*g)[child][j] = plant
		}
	}
	return nil
}

func lookupPlant(code byte) (plant string, err error) {
	switch code {
	case 'C':
		plant = "clover"
	case 'G':
		plant = "grass"
	case 'R':
		plant = "radishes"
	case 'V':
		plant = "violets"
	default:
		err = fmt.Errorf("invalid plant code '%c'", code)
	}
	return
}

/* bench
 * BenchmarkNewGarden     	   65277	     19705 ns/op	    9491 B/op	     122 allocs/op
 * BenchmarkGarden_Plants 	 4577480	       236.6 ns/op	       0 B/op	       0 allocs/op
 */
