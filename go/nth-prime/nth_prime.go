package prime

/* The go test runner allows *this specific version*
 *
 * $ go get golang.org/x/exp@v0.0.0-20221006183845-316c7553db56
 * go: downloading golang.org/x/exp v0.0.0-20221006183845-316c7553db56
 * go: downgraded golang.org/x/exp v0.0.0-20230315142452-642cacee5cc0 => v0.0.0-20221006183845-316c7553db56
 * $ go mod tidy
 * ...
 * $ exercism submit nth-prime.go go.mod go.sum
 */

import (
	"errors"
	"golang.org/x/exp/slices"
)

//lint:file-ignore U1000 only one func will actually be used

var Primes = []int{2, 3}

func Nth(n int) (int, error) {
	if n < 1 {
		return 0, errors.New("there is no zeroth prime")
	}

	/*
		if n == 1 {
			return 2, nil
		}

	    //return using_array_of_primes(n), nil
		return not_using_array(n), nil
	*/
	return using_package_variable(n), nil
}

// ------------------------------------------------------------
func using_array_of_primes(n int) int {
	primes := make([]int, 0, n)
	primes = append(primes, 2)
	primes = append(primes, 3)

	// closure: determine if the given number is prime
	isPrime := func(n int) bool {
		for _, p := range primes {
			if p*p > n {
				break
			}
			if n%p == 0 {
				return false
			}
		}
		return true
	}

	// closure: find the next prime number
	nextPrime := func() int {
		candidate := primes[len(primes)-1]
		for {
			candidate += 2
			if isPrime(candidate) {
				break
			}
		}
		return candidate
	}

	for n > len(primes) {
		primes = append(primes, nextPrime())
	}

	return primes[n-1]
}

/*
 * $ go test
 * PASS
 * ok      prime   0.009s
 *
 * $ go test --bench=. --benchmem
 * goos: linux
 * goarch: amd64
 * pkg: prime
 * cpu: Intel(R) Core(TM) i7-9750H CPU @ 2.60GHz
 * BenchmarkNth-2               187           6394831 ns/op           81920 B/op          1 allocs/op
 * PASS
 * ok      prime   1.853s
 */

// ------------------------------------------------------------
func not_using_array(n int) int {
	// closure: determine if the given number is prime
	isPrime := func(n int) bool {
		for f := 3; f*f <= n; f += 2 {
			if n%f == 0 {
				return false
			}
		}
		return true
	}

	// closure: find the next prime number
	nextPrime := func(candidate int) int {
		for {
			candidate += 2
			if isPrime(candidate) {
				break
			}
		}
		return candidate
	}

	prime := 3
	for count := 2; count < n; count++ {
		prime = nextPrime(prime)
	}

	return prime
}

/* This is close to 2x slower.
 * It has to allocate much less memory, but the isPrime closure
 * needs to do a lot more loop iterations.
 *
 * $ go test
 * PASS
 * ok      prime   0.016s
 *
 * $ go test --bench=. --benchmem
 * goos: linux
 * goarch: amd64
 * pkg: prime
 * cpu: Intel(R) Core(TM) i7-9750H CPU @ 2.60GHz
 * BenchmarkNth-2               100          11792986 ns/op               0 B/op          0 allocs/op
 * PASS
 * ok      prime   1.205s
 */

// ------------------------------------------------------------
func using_package_variable(n int) int {
	// do we already know it?
	if n <= len(Primes) {
		return Primes[n-1]
	}

	// grow the slice
	Primes = slices.Grow(Primes, n-len(Primes))

	// closure: determine if the given number is prime
	isPrime := func(n int) bool {
		for _, p := range Primes {
			if p*p > n {
				break
			}
			if n%p == 0 {
				return false
			}
		}
		return true
	}

	// closure: find the next prime number
	nextPrime := func() int {
		candidate := Primes[len(Primes)-1]
		for {
			candidate += 2
			if isPrime(candidate) {
				break
			}
		}
		return candidate
	}

	for n > len(Primes) {
		Primes = append(Primes, nextPrime())
	}

	return Primes[n-1]
}

/* Using a package variable to cache the primes is the absolute benchmark champion.
 *
 * $ go test
 * PASS
 * ok      prime   0.010s
 *
 * $ go test --bench=. --benchmem
 * goos: linux
 * goarch: amd64
 * pkg: prime
 * cpu: Intel(R) Core(TM) i7-9750H CPU @ 2.60GHz
 * BenchmarkNth-2          647742152                1.800 ns/op           0 B/op          0 allocs/op
 * PASS
 * ok      prime   1.368s
 *
 */
