package armstrong

import "math"

func IsNumber(n int) bool {
	var (
		size = int(math.Ceil(math.Log10(float64(n))))
		sum  = 0
	)

	for m := n; m > 0; m /= 10 {
		sum += pow(m%10, size)
	}

	return sum == n
}

// an integer math.Pow(x,y)
func pow(x, y int) (result int) {
	result = 1
	for ; y > 0; y-- {
		result *= x
	}
	return
}

// bench
// BenchmarkIsNumber-2      4741692               244.6 ns/op             0 B/op          0 allocs/op
