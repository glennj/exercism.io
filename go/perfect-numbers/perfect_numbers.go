package perfect

import (
	"errors"
	"math"
)

type Classification int

const (
	// note to self: https://go.dev/ref/spec#Iota
	ClassificationDeficient Classification = iota + 1
	ClassificationPerfect
	ClassificationAbundant
)

var ErrOnlyPositive = errors.New("positive numbers only")

func Classify(n int64) (class Classification, err error) {
	if n < 1 {
		err = ErrOnlyPositive
		return
	}

	sum := aliquotSum(n)
	switch {
	case sum < n:
		class = ClassificationDeficient
	case sum > n:
		class = ClassificationAbundant
	default:
		class = ClassificationPerfect
	}
	return
}

func aliquotSum(n int64) int64 {
	var sum int64

	for i := int64(math.Sqrt(float64(n))); i >= 1; i-- {
		if n%i == 0 {
			sum += i
			if i != n/i {
				sum += n / i
			}
		}
	}

	return sum - n
}

/* benchmarks
 * BenchmarkClassify          10000            109458 ns/op               0 B/op          0 allocs/op
 */
