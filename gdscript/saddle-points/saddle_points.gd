func saddle_points(matrix):
	if matrix.is_empty(): return []

	var columns = range(matrix[0].size()) \
			.map(func(c): return matrix.map(func(row): return row[c]))

	var row_maxima = matrix.map(func(row): return row.max())
	var col_minima = columns.map(func(col): return col.min())

	var sps = []

	for r in range(matrix.size()):
		for c in range(matrix[r].size()):
			var val = matrix[r][c]
			if val == row_maxima[r] and val == col_minima[c]:
				sps.append([r + 1, c + 1])

	return sps
