package sieve

// Collander is a manifestation of a "sieve" (too bad the word Sieve is taken).
type Collander []bool

func Sieve(limit int) []int {
	// 1. create the sieve
	sieve := newCollander(limit)

	// 2. mark the non-primes (multiples of the current prime
	for prime := 2; prime*prime <= limit; prime = sieve.nextPrime(prime) {
		sieve.markMultiples(prime)
	}

	// 3. collect the prime numbers
	// How many will there be? Let's guess.
	primes := make([]int, 0, limit/2)
	for i, isPrime := range sieve {
		if isPrime {
			primes = append(primes, i)
		}
	}
	return primes
}

func newCollander(limit int) Collander {
	sieve := make(Collander, limit+1)
	sieve[0] = false // 0 is not prime
	sieve[1] = false // 1 is not prime
	for i := 2; i <= limit; i++ {
		sieve[i] = true // primality TBD
	}
	return sieve
}

func (s Collander) markMultiples(prime int) {
	var step int
	if prime == 2 {
		step = 2
	} else {
		step = prime * 2
	}
	for idx := prime * prime; idx < len(s); idx += step {
		s[idx] = false
	}
}

func (s Collander) nextPrime(prev int) int {
	for idx := prev + 1; idx < len(s); idx++ {
		if s[idx] {
			// I'm a prime number
			return idx
		}
	}
	return len(s)
}

/* benchmarks
 *
 * BenchmarkSieve-2          536972              2330 ns/op            5264 B/op          9 allocs/op
 */
