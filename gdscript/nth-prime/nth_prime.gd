var primes = [2, 3]

func prime(n):
	if n < 1:
		return null

	var is_prime = func(candidate):
		var limit = sqrt(candidate)
		for p in primes:
			if p > limit:
				break
			if candidate % p == 0:
				return false
		return true

	var next_prime = func():
		var m = primes[-1] + 2
		while not is_prime.call(m):
			m += 2
		return m

	while primes.size() < n:
		primes.append(next_prime.call())

	return primes[n - 1]
