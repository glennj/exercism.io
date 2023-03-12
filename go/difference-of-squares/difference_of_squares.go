package diffsquares

func SquareOfSum(n int) int {
	var sum int
    /*
    for i := 1; i <= n; i++ {
        sum += i
    }
    */

    // sum: n(n+1)/2
    sum = n * (n + 1) / 2

	return sum * sum
}

func SumOfSquares(n int) int {
	var sum int
    /*
    for i := 1; i <= n; i++ {
        sum += i * i
    }
    */

    // sum of squares: n(n+1)(2n+1)/6
    sum = n * (n + 1) * (2 * n + 1) / 6

	return sum
}

func Difference(n int) int {
	return SquareOfSum(n) - SumOfSquares(n)
}

/* benchmarks
 *
 * using loops
 * BenchmarkSquareOfSum-2          15358201                67.61 ns/op            0 B/op          0 allocs/op
 * BenchmarkSumOfSquares-2         17112925                67.21 ns/op            0 B/op          0 allocs/op
 * BenchmarkDifference-2            8077932               147.1 ns/op             0 B/op          0 allocs/op
 *
 * using formulae
 * BenchmarkSquareOfSum-2          1000000000               1.084 ns/op           0 B/op          0 allocs/op
 * BenchmarkSumOfSquares-2         1000000000               0.5439 ns/op          0 B/op          0 allocs/op
 * BenchmarkDifference-2           1000000000               1.084 ns/op           0 B/op          0 allocs/op
 */
