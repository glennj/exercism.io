package spiralmatrix

func SpiralMatrix(size int) [][]int {

	var (
		spiral = make([][]int, size)

		x, y   = -1, 0
		dx, dy = 1, 0

		turn = func() bool {
			var (
				nextX = x + dx
				nextY = y + dy
			)
			return nextX < 0 || nextX == size ||
				nextY < 0 || nextY == size ||
				spiral[nextY][nextX] != 0
		}
	)
	for i := 0; i < size; i++ {
		spiral[i] = make([]int, size)
	}

	for i := 0; i < size*size; i++ {
		if turn() {
			dx, dy = -dy, dx
		}
		x += dx
		y += dy
		spiral[y][x] = i + 1
	}

	return spiral
}

// bench
// BenchmarkSpiralMatrix-2   	 1916598	       566.9 ns/op	     488 B/op	      14 allocs/op
