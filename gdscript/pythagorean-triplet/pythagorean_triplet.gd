func triplets_with_sum(n):
	var triplets = []

	# start with shortest side of smallest triplet [3, 4, 5]
	for a in range(3, n):

		# formula for b derived from:
		#    a + b + c == n
		#    a^2 + b^2 == c^2
		# combined, we see
		#    a^2 + b^2 == (n - a - b)^2
		# solve for b
		
		var b = 1.0 * n * (n - 2 * a) / (2 * (n - a))

		if b < a:
			break

		var bi = int(b)
		if b == bi:
			triplets.append([a, bi, n - a - bi])

	return triplets
