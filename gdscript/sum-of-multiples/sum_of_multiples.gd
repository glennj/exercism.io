func sum(factors: Array, limit: int):
	# remove 0 as a factor
	factors.erase(0)
	if factors.is_empty(): return 0

	# find the multiples of the factors
	var multiples = range(1, limit) \
			.filter(func (m): return factors.any(func (f): return m % f == 0))

	# and return the sum
	return array_sum(multiples)


func array_sum(a: Array):
	return a.reduce(func (sum, n): return sum + n, 0)
