func spiral_matrix(size):
	var mtx = []
	for i in range(size):
		var row = []
		row.resize(size)
		mtx.append(row)

	var cell = Vector2i(0, 0)
	var delta = Vector2i(0, 1)

	var out_of_range = func(v):
		return v.x < 0 or v.x >= size or v.y < 0 or v.y >= size

	for i in range(1, size**2 + 1):
		mtx[cell.x][cell.y] = i

		var v = cell + delta
		if out_of_range.call(v) or mtx[v.x][v.y] != null:
			delta = Vector2i(delta.y, -delta.x)

		cell += delta

	return mtx
