func combinations(sum, size, exclude):
	if size == 1:
		if sum < 1 or sum > 9 or exclude.has(sum):
			return []
		return [[sum]]

	var combos = []
	for d in range(1, 10):
		if not exclude.has(d):
			for c in combinations(sum - d, size - 1, exclude + [d]):
				var combo = c + [d]
				combo.sort()
				if not combos.has(combo):
					combos.append(combo)

	return combos
