"""
Using the Binary numeral system (base 2) from Wikipedia
https://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Binary_numeral_system_%28base_2%29
"""
func square_root(n: int):
	# find b, the greatest power of 4 <= n
	var b = 4 ** int(log(n) / log(4))
	var x = 0

	while b != 0:
		if n >= x + b:
			n -= x + b
			x = (x >> 1) + b
		else:
			x >>= 1
		b >>= 2

	return x
