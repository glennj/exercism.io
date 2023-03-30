package allergies

var allergens = []string{
	"eggs",
	"peanuts",
	"shellfish",
	"strawberries",
	"tomatoes",
	"chocolate",
	"pollen",
	"cats",
}

func Allergies(allergies uint) []string {
	result := make([]string, 0, len(allergens))
	for i, a := range allergens {
		if isAllergicAt(allergies, i) {
			result = append(result, a)
		}
	}
	return result
}

func AllergicTo(allergies uint, allergen string) bool {
	for i, a := range allergens {
		if a == allergen && isAllergicAt(allergies, i) {
			return true
		}
	}
	return false
}

func isAllergicAt(allergies uint, idx int) bool {
	x := allergies & (1 << idx)
	return x > 0
}

/* bench
 * BenchmarkAllergies        307887              3935 ns/op            5120 B/op         40 allocs/op
 * BenchmarkAllergicTo      1841826               615.1 ns/op             0 B/op          0 allocs/op
 */
