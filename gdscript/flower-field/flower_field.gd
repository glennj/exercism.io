const FLOWER = 99
const SPACE = 0


func annotate(garden):
	if garden.is_empty(): return []
	if garden.size() == 1 and garden[0].is_empty(): return [""]

	var m = to_matrix(garden)

	var valid_indices = func(i, j):
		return 0 <= i and i < m.size() and 0 <= j and j < m[0].size()

	for i in range(m.size()):
		for j in range(m[0].size()):
			if m[i][j] != FLOWER:
				for di in [-1, 0, 1]:
					for dj in [-1, 0, 1]:
						if (di or dj) \
								and valid_indices.call(i + di, j + dj) \
								and m[i + di][j + dj] == FLOWER:
							m[i][j] += 1

	return to_annotated(m)


func to_matrix(garden):
	return garden.map(func (row):
		return Array(row.split()).map(func (c): return FLOWER if c == "*" else SPACE))


func to_annotated(matrix):
	return matrix.map(func (row):
		return "".join(row.map(func (n):
			if n == FLOWER: return "*"
			elif n == SPACE: return " "
			else: return str(n))))
