let primes = limit => {
	let marks = Array.concat([false, false], Array.make(~length=limit - 1, true))

	let rec markMultiples = (m, step) => {
		if m <= limit {
			marks[m] = false
			markMultiples(m + step, step)
		}
	}

	// mark multiples of 2 as non-primes
	markMultiples(2 * 2, 2)

	// for each odd number that is prime, mark its multiples as non-prime
	let rec primesIter = (n) => {
		if n * n <= limit {
			if marks->Array.getUnsafe(n) {
				markMultiples(n * n, n * 2)
			}
			primesIter(n + 2)
		}
	}
	primesIter(3)

	// extract the primes from the marks array
	marks
	->Array.mapWithIndex((isPrime, i) => (isPrime, i))
	->Array.filter(((isPrime, _)) => isPrime)
	->Array.map(((_, i)) => i)
}
