func classify(number: int):
	if number < 1: return null

	var sum = aliquot_sum(number)

	if   sum < number: return "deficient"
	elif sum > number: return "abundant"
	else:              return "perfect"


func aliquot_sum(n):
	var factors = {}

	for i in range(1, int(sqrt(n)) + 1):
		if n % i == 0:
			factors[i] = true
			factors[n / i] = true

	var add = func(a, b): return a + b
	return factors.keys().reduce(add, 0) - n
	
