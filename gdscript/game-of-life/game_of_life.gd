func tick(matrix):
	if matrix.is_empty(): return []

	var height = matrix.size()
	var width  = matrix[0].size()

	var result = range(height).map(func (x):
			var row = []
			row.resize(width)
			row.fill(0)
			return row)

	for x in range(height):
		for y in range(width):
			if matrix[x][y] == 1:
				for n in neighbour_indices(x, y, height, width):
					result[n.x][n.y] += 1

	for x in range(height):
		for y in range(width):
			match result[x][y]:
				3: result[x][y] = 1
				2: result[x][y] = matrix[x][y]
				_: result[x][y] = 0

	return result


const DELTAS = [[-1, -1], [-1, 0], [-1, 1], [0, -1], [0, 1], [1, -1], [1, 0], [1, 1]]

func neighbour_indices(i, j, height, width):
	var ns = []
				
	for d in DELTAS:
		if 0 <= i + d[0] and i + d[0] < height \
			and 0 <= j + d[1] and j + d[1] < width:
				ns.append({x = i + d[0], y = j + d[1]})
	return ns
