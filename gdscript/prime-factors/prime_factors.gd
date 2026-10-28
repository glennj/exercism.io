func factors(number: int) -> Array:
	var fs = []
	var f = 2

	while f * f <= number:
		while number % f == 0:
			fs.append(f)
			number /= f
		f += 1
	
	if number > 1:
		fs.append(number)

	return fs
