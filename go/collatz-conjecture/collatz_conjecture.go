package collatzconjecture

import "errors"

func CollatzConjecture(n int) (int, error) {
	if n <= 0 {
		return 0, errors.New("must be greater than zero")
	}

	return iterative(n), nil
	//return recursive(n, 0), nil
}

func iterative(n int) int {
	steps := 0
	for n != 1 {
		if n%2 == 0 {
			n = n / 2
		} else {
			n = 3*n + 1
		}
		steps++
	}
	return steps
}

func recursive(n, steps int) int {
	if n == 1 {
		return steps
	} else if n%2 == 0 {
		return recursive(n/2, steps+1)
	} else {
		return recursive(3*n+1, steps+1)
	}
}

/* benchmarks
 *
 * iterative
 * BenchmarkCollatzConjecture       4506798               266.5 ns/op             0 B/op          0 allocs/op
 *
 * recursive
 * BenchmarkCollatzConjecture       1304869               924.5 ns/op             0 B/op          0 allocs/op
 */
