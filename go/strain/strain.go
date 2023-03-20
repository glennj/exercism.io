package strain

type Ints []int
type Lists [][]int
type Strings []string

// a lot of cut and paste here

func (i Ints) Keep(filter func(int) bool) (result Ints) {
    for _, elem := range i {
        if filter(elem) {
            result = append(result, elem)
        }
    }
    return result
}

func (i Ints) Discard(filter func(int) bool) (result Ints) {
    for _, elem := range i {
        if !filter(elem) {
            result = append(result, elem)
        }
    }
    return result
}

func (l Lists) Keep(filter func([]int) bool) (result Lists) {
    for _, elem := range l {
        if filter(elem) {
            result = append(result, elem)
        }
    }
    return result
}

func (s Strings) Keep(filter func(string) bool) (result Strings) {
    for _, elem := range s {
        if filter(elem) {
            result = append(result, elem)
        }
    }
    return result
}

/* benchmarks
 * BenchmarkKeepInts-2              4741183               277.0 ns/op           104 B/op          7 allocs/op
 *
 * BenchmarkDiscardInts-2           4397242               272.5 ns/op           120 B/op          7 allocs/op
 */
