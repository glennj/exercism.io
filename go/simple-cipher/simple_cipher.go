package cipher

func NewCaesar() Cipher {
	return shift{Distance: 3}
}

func floorMod(n int, div int) int {
	return (n%div + div) % div
}

/* benchmarks
 *
 * goos: linux
 * goarch: amd64
 * pkg: cipher
 * cpu: Intel(R) Core(TM) i7-9750H CPU @ 2.60GHz
 * BenchmarkEncodeCaesar
 * BenchmarkEncodeCaesar-2          1307528               878.4 ns/op           264 B/op         10 allocs/op
 * BenchmarkDecodeCaesar
 * BenchmarkDecodeCaesar-2          1708311               725.5 ns/op           120 B/op          5 allocs/op
 * BenchmarkNewShift
 * BenchmarkNewShift-2             73890987                15.74 ns/op            0 B/op          0 allocs/op
 * BenchmarkEncodeShift
 * BenchmarkEncodeShift-2            572089              2093 ns/op             536 B/op         17 allocs/op
 * BenchmarkDecodeShift
 * BenchmarkDecodeShift-2            733382              1508 ns/op             272 B/op          9 allocs/op
 * BenchmarkNewVigenere
 * BenchmarkNewVigenere-2          18461257                57.44 ns/op            0 B/op          0 allocs/op
 * BenchmarkEncVigenere
 * BenchmarkEncVigenere-2            704113              1457 ns/op             288 B/op         20 allocs/op
 * BenchmarkDecVigenere
 * BenchmarkDecVigenere-2           1000000              1039 ns/op             160 B/op         13 allocs/op
 * PASS
 * ok      cipher  13.066s
 */
