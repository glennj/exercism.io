func nucleotide_counts(strand):
	var counts = {A = 0, C = 0, G = 0, T = 0}

	for c in strand:
		if not counts.has(c):
			return null
		counts[c] += 1

	return counts

