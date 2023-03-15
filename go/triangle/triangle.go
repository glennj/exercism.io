package triangle

type Kind int

const (
	NaT Kind = iota
	Equ
	Iso
	Sca
)

// KindFromSides should have a comment documenting it. Yes indeed.
func KindFromSides(a, b, c float64) Kind {
	switch {
	case a <= 0 || b <= 0 || c <= 0:    fallthrough
	case a > b+c || b > a+c || c > a+b: return NaT
	case a == b && b == c:              return Equ
	case a == b || b == c || c == a:    return Iso
	default:                            return Sca
	}
}

/* benchmark
 * BenchmarkKind   143146732                8.532 ns/op           0 B/op          0 allocs/op
 */
