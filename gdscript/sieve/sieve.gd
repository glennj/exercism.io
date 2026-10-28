# ref: https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes#Pseudocode
func primes(limit: int):

	# let A be an array of Boolean values, indexed by integers 2 to n,
    # initially all set to true.
	var A = []
	A.resize(limit + 1)
	A.fill(true)
	A[0] = false
	A[1] = false

	# mark multiples of primes as non-prime
	for i in range(2, int(sqrt(limit)) + 1):
		if A[i]:
			for j in range(i**2, limit + 1, i):
				A[j] = false

	# return all i such that A[i] is true.
	return range(A.size()) \
			.map(func(i): return [i, A[i]]) \
			.filter(func(pair): return pair[1]) \
			.map(func(pair): return pair[0])
